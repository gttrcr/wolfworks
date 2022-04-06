#pragma once

#include "../core/ssh.h"
#include "../core/cmds.h"
#include "../core/config.h"
#include "pwpf.h"
#include "pwsd.h"
#include "scriptexec.h"
#include "error.h"

class scripter
{
private:
	using sshScriptMapT = std::unordered_map<ssh*, std::vector<scriptexec>>;
	using pairT = std::pair<ssh*, std::vector<scriptexec>>;
	sshScriptMapT* _sshScriptMap;
	std::vector<pwpf> _estimation;
	
	//async call return the pid of the process
	unsigned int sendAndStartAsync(ssh* s, const std::string& scriptContent, err& err, std::string ts = "")
	{
		//create file
		if (ts == "")
			ts = utils::timeStamp();

		std::string inputRemoteFile = utils::getWorkingDir() + FOLDER_INPUT + ts;
		std::string outputRemoteFile = utils::getWorkingDir() + FOLDER_OUTPUT + ts;
		std::string piwolfpidscript = utils::getWorkingDir() + FILE_PIWOLF_PID_SCRIPT;

		//transfer file
		err_n err_n = s->transferFile(inputRemoteFile, scriptContent);
		err.add(err_n);

		if (err_n == err_n::SFTP_OK)
		{
			//start execution
			std::string command =
				utils::replaceStringWithString(
					utils::replaceStringWithString(
						utils::replaceStringWithString(
							utils::replaceStringWithString(CMD_START_ASYNC_WOLFRAM_KERNEL,
								PLACEHOLDER0, inputRemoteFile),
							PLACEHOLDER1, outputRemoteFile),
						PLACEHOLDER2, piwolfpidscript),
					PLACEHOLDER3, ts);

			std::string result;
			err_n = s->remoteCommand(command, result);
			err.add(err_n);

			if (err_n == err_n::REMOTE_COMMAND_OK)
			{
				std::vector<std::string> res = utils::splitStringByChar(result);
				if (res.size() > 0)
					return std::stoi(res[0]);
			}
		}

		//the only process with pid 0 is the scheduler (if return 0, then error)
		return 0;
	}

	//sync call return the result of computation line by line
	err_n sendAndStartSync(ssh* s, const std::string& scriptContent, std::vector<std::string>& res, err& err)
	{
		//create file
		std::string ts = utils::timeStamp();
		std::string inputRemoteFile = utils::getWorkingDir() + FOLDER_INPUT + ts;
		std::string outputRemoteFile = utils::getWorkingDir() + FOLDER_OUTPUT + "out_" + ts;

		//transfer file
		err_n err_n = s->transferFile(inputRemoteFile, scriptContent);
		err.add(err_n);
		
		if (err_n == err_n::SFTP_OK)
		{
			//start execution
			std::string command = utils::replaceStringWithString(utils::replaceStringWithString(CMD_START_SYNC_WOLFRAM_KERNEL,
				PLACEHOLDER0,
				inputRemoteFile),
				PLACEHOLDER1,
				outputRemoteFile);

			std::string result;
			err_n = s->remoteCommand(command, result);
			err.add(err_n);
			
			if (err_n == err_n::REMOTE_COMMAND_OK)
			{
				res = utils::splitStringByChar(result);
				return err_n::SEND_AND_START_SYNC_OK;
			}
			else
				return err_n::REMOTE_COMMAND_REQUEST_KO;
		}
		else
			return err_n::SFTP_KO;
	}

public:
	scripter()
	{
		_sshScriptMap = new sshScriptMapT();
	}
	
	bool tryAdd(ssh* s, const std::string& scriptPath, err& err, const std::string& ts = "", const std::string& asyncTimeEstimation = "")
	{
		//if map contains ssh the update else insert
		sshScriptMapT::iterator it = _sshScriptMap->find(s);
		unsigned int pid = 0;
		if (it != _sshScriptMap->end())
		{
			if (it->first->availableForComputation())
			{
				std::string scriptContent = utils::readFile(scriptPath);
				pid = sendAndStartAsync(s, scriptContent, err, ts);
				if (pid == 0)
					return false;

				it->second.push_back(scriptexec(pid, scriptContent));
			}
			else
				return false;
		}
		else
		{
			std::string scriptContent = utils::readFile(scriptPath);
			pid = sendAndStartAsync(s, scriptContent, err, ts);
			if (pid == 0)
				return false;

			std::vector<scriptexec> sev;
			sev.push_back(scriptexec(pid, scriptContent));
			pairT pair(s, sev);
			_sshScriptMap->insert(pair);
		}

		//save the estimated time to complete the task
		if (asyncTimeEstimation != "")
		{
			if (!utils::checkFileExists(FILE_PIWOLF_PID_ESTIMATED))
				utils::writeFile(FILE_PIWOLF_PID_ESTIMATED, "");
			std::string content = utils::readFile(FILE_PIWOLF_PID_ESTIMATED);
			content += std::to_string(pid) + " " + asyncTimeEstimation + "\n";
			utils::writeFile(FILE_PIWOLF_PID_ESTIMATED, content);
		}

		return true;
	}
	
	bool tryAdd(std::vector<ssh*> sshs, err& err)
	{
		if (_estimation.size() == 0)
			return false;

		std::string ts = utils::timeStamp();
		bool ret = true;
		unsigned int tsIndex = 0;
		for (unsigned int i = 0; i < sshs.size(); i++)
			for (unsigned int a = 0; a < sshs[i]->coresAvailable() && tsIndex < _estimation.size(); a++)
			{
				std::string correctedTs = ts + "_" + std::to_string(tsIndex);
				std::string content = utils::replaceStringWithString(_estimation[tsIndex].comp.script, _estimation[tsIndex].comp.vars.name + "Start", _estimation[tsIndex].comp.vars.start);
				content = utils::replaceStringWithString(content, _estimation[tsIndex].comp.vars.name + "Stop", _estimation[tsIndex].comp.vars.stop);
				tsIndex++;
				utils::writeFile(correctedTs, content);
				ret &= tryAdd(sshs[i], correctedTs, err, correctedTs, _estimation[tsIndex].comp.vars.estimation);
				utils::removeFile(correctedTs);
			}

		return ret;
	}
	
	err_n estimation(ssh* s, unsigned int selectedCores, const pwpf& pwpfObj, err& err)
	{
		try
		{
			_estimation.clear();

			bool storeFileExists = utils::checkFileExists(config::getInstance().storeFile());
			if (storeFileExists)
			{
				pwsd pwsdObj;
				if (pwsdObj.stringToObj(utils::readFile(config::getInstance().storeFile())))
				{
					for (unsigned int i = 0; i < pwsdObj.est.size(); i++)
					{
						if (pwsdObj.est[i].script == pwpfObj.test.script && pwsdObj.est[i].vars == pwpfObj.test.vars && pwsdObj.est[i].res.start.size() == selectedCores)
						{
							//get results from storeFile
							pwsd::result res = pwsdObj.est[i].res;

							//Create the final parallel format
							pwpf pw;
							pw.comp.script = pwpfObj.comp.script;
							for (int r = 0; r < res.start.size(); r++)
							{
								pwpf::computationVar cVar;
								cVar.name = pwpfObj.comp.vars.name;
								cVar.start = res.start[r];
								cVar.stop = res.stop[r];
								//TODO - add estimation (see pwpf)
								pw.comp.vars = cVar;
								_estimation.push_back(pw);
							}

							//estimation has already been loaded, nothing else to do
							return ESTIMATION_OK;
						}
					}
				}
			}

			//only the first variable will be evaluated (TODO - extend to C^n)
			for (unsigned int i = 0; i < 1 /*pwpfObj.test.vars.size()*/; i++)
			{
				//create the complexity fit
				std::string data = "";
				for (unsigned int a = 0; a < pwpfObj.test.vars.values.size(); a++)
				{
					std::string lineAbsTiming = utils::replaceStringWithString(pwpfObj.test.script,
						pwpfObj.test.vars.name,
						pwpfObj.test.vars.values[a]);
				
					lineAbsTiming = utils::replaceStringWithString(WOLFRAM_ABSOLUTE_TIMING, PLACEHOLDER0, lineAbsTiming);
					lineAbsTiming = utils::replaceStringWithString(WOLFRAM_FIRST, PLACEHOLDER0, lineAbsTiming);
					data += "{" + pwpfObj.test.vars.values[a] + ", " + lineAbsTiming + "}, ";
				}
				data = data.substr(0, data.length() - 2);
				std::string complexityFit = utils::readFile(utils::getWorkingDir() + FOLDER_PIWOLF_SCRIPT + COMPLEXITY_FIT);
				complexityFit = utils::replaceStringWithString(complexityFit, PLACEHOLDER0, data);
				complexityFit = utils::replaceStringWithString(complexityFit, PLACEHOLDER1, std::to_string(selectedCores));
				complexityFit = utils::replaceStringWithString(complexityFit, PLACEHOLDER2, pwpfObj.test.vars.from);
				complexityFit = utils::replaceStringWithString(complexityFit, PLACEHOLDER3, pwpfObj.test.vars.to);

				//execute complexity fit
				std::vector<std::string> res;
				err_n err_n = sendAndStartSync(s, complexityFit, res, err);
				err.add(err_n);
				if (err_n != err_n::SEND_AND_START_SYNC_OK)
					return err_n;

				//create the final parallel format
				pwpf pwpfFinal;
				std::string estimation = res[0];
				pwpfFinal.comp.script = pwpfObj.comp.script;
				for (unsigned int r = 1; r < res.size() - 1; r++)
				{
					pwpf::computationVar cVar;
					cVar.name = pwpfObj.comp.vars.name;
					cVar.start = res[r];
					cVar.stop = res[r + 1];
					cVar.estimation = estimation;
					pwpfFinal.comp.vars = cVar;
					_estimation.push_back(pwpfFinal);
				}

				pwsd::estimation es;
				es.script = pwpfObj.test.script;
				es.vars = pwpfObj.test.vars;
				es.res.name = es.vars.name;
				for (unsigned int r = 0; r < res.size() - 1; r++)
					es.res.start.push_back(res[r]);
				for (unsigned int r = 1; r < res.size(); r++)
					es.res.stop.push_back(res[r]);

				//when I am here, is storeFileExist then I will add the new estimation, else I will create the file and add the first estimation
				if (storeFileExists)
				{
					std::string storeFile = utils::readFile(config::getInstance().storeFile());
					pwsd pwsdFile;
					pwsdFile.stringToObj(storeFile);
					pwsdFile.est.push_back(es);
					json pwsdJson = pwsdFile.objToJson();
					utils::writeFile(config::getInstance().storeFile(), pwsdJson.dump(4));
				}
				else
				{
					pwsd pwsdFile;
					pwsdFile.est.push_back(es);
					json content = pwsdFile.objToJson();
					utils::writeFile(config::getInstance().storeFile(), content.dump(4));
				}
			}

			return ESTIMATION_OK;
		}
		catch (std::exception& ex)
		{
			return ESTIMATION_KO;
		}
	}
	
	void setPid(ssh* s, std::vector<unsigned int>& diff)
	{
		
	}
	
	void unsetPid(std::vector<unsigned int>& diff)
	{
		
	}
};