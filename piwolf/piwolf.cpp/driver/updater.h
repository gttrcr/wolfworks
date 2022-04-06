#pragma once

#include <vector>
#include <thread>

#include "../core/ssh.h"
#include "../core/cmds.h"

class updater
{
	friend class task;

private:
	std::vector<ssh*>* _sshs;
	scripter* _scripter;
	unsigned int _setPidCount;
	unsigned int _unsetPidCount;
	
public:
	void refresh()
	{
		for (unsigned int i = 0; i < _sshs->size(); i++)
		{
			ssh* current = _sshs->at(i);
			if (current->_isConnected)
			{
				std::string res;
				err_n err_n = current->remoteCommand(CMD_OS_RESOURCES, res);
				if (err_n == err_n::REMOTE_COMMAND_OK)
				{
					std::vector<std::string> vRes = utils::splitStringByChar(res);
					current->_coreNumber = std::stoi(vRes[0]);
					if (current->_cpuLoad == nullptr)
						current->_cpuLoad = new std::vector<double>(current->_coreNumber);
					for (unsigned int i = 0; i < current->_coreNumber; i++)
						current->_cpuLoad->at(i) = std::round(std::stod(vRes[1 + i]) * 100) / 100;

					if (utils::stringEndsWithString(vRes[current->_coreNumber + 1], "Mi"))
						current->_ramLoad = std::round(std::stod(utils::replaceStringWithString(vRes[current->_coreNumber + 1], "Mi")) * 100) / 100;
					else if (utils::stringEndsWithString(vRes[current->_coreNumber + 1], "Gi"))
						current->_ramLoad = std::round(std::stod(utils::replaceStringWithString(vRes[current->_coreNumber + 1], "Gi")) * 1000 * 100) / 100;

					current->_cpuTemp = vRes[current->_coreNumber + 2];
					if (current->_throttled == nullptr)
						current->_throttled = new throttled();
					current->_throttled->throttledValue(vRes[current->_coreNumber + 3]);

					err_n = current->remoteCommand(CMD_PID_OF_KERNELS, res);
					if (err_n == err_n::REMOTE_COMMAND_OK)
					{
						std::vector<std::string> res1 = utils::splitStringByChar(res);
						err_n = current->remoteCommand(CMD_PID_OF_KERNELS, res);
						if (err_n == err_n::REMOTE_COMMAND_OK)
						{
							std::vector<std::string> res2 = utils::splitStringByChar(res);
							std::sort(res1.begin(), res1.end());
							std::sort(res2.begin(), res2.end());
							std::vector<std::string> common;
							std::set_intersection(res1.begin(), res1.end(), res2.begin(), res2.end(), std::back_inserter(common));

							//copy _pidOfKernels in _lastPidOfKernels
							current->_lastPidOfKernels->clear();
							for (unsigned p = 0; p < current->_pidOfKernels->size(); p++)
								current->_lastPidOfKernels->push_back(current->_pidOfKernels->at(p));

							current->_pidOfKernels->clear();
							for (unsigned int p = 0; p < common.size(); p++)
								current->_pidOfKernels->push_back(std::stoi(common[p]));

							//set pid of the new kernel
							if (current->_lastPidOfKernels->size() < current->_pidOfKernels->size())
							{
								std::vector<unsigned int> diff;
								std::set_difference(current->_pidOfKernels->begin(), current->_pidOfKernels->end(), current->_lastPidOfKernels->begin(), current->_lastPidOfKernels->end(), std::inserter(diff, diff.begin()));
								_scripter->setPid(current, diff);

								if (_setPidCount > 0)
									_setPidCount -= diff.size();
							}

							//unset pid of a terminated kernel
							if (current->_lastPidOfKernels->size() > current->_pidOfKernels->size())
							{
								std::vector<unsigned int> diff;
								std::set_difference(current->_lastPidOfKernels->begin(), current->_lastPidOfKernels->end(), current->_pidOfKernels->begin(), current->_pidOfKernels->end(), std::inserter(diff, diff.begin()));
								_scripter->unsetPid(diff);

								if (_unsetPidCount > 0)
									_unsetPidCount -= diff.size();
							}

							//for every running process, get the progress value
							for (unsigned int p = 0; p < current->_pidOfKernels->size(); p++)
							{
								unsigned int pid = current->_pidOfKernels->at(p);
								std::string cmd = utils::replaceStringWithString(utils::replaceStringWithString(CMD_PROGRESS_OF_RUNNING,
									PLACEHOLDER0, std::to_string(pid)),
									PLACEHOLDER1, utils::getWorkingDir());
								err_n = current->remoteCommand(cmd, res);
								if (err_n == err_n::REMOTE_COMMAND_OK)
								{
									if (res == "")
										res = "0";
									current->setProgress(pid, std::stod(res) / 1000000000);
								}
								else
									std::cout << "Error " << err_n << std::endl;
							}
						}
						else
							std::cout << "Error " << err_n << std::endl;
					}
					else
						std::cout << "Error " << err_n << std::endl;
				}
				else
					std::cout << "Error " << err_n << std::endl;
			}
			else
				current->connect();
		}
	}
	
	updater(scripter* scripter)
	{
		_sshs = new std::vector<ssh*>();
		_scripter = scripter;

		for (unsigned int i = 0; i < config::getInstance().nodeList()->size(); i++)
		{
			std::string ip = config::getInstance().nodeList()->at(i).ip;
			std::string user = config::getInstance().nodeList()->at(i).user;
			std::string password = config::getInstance().nodeList()->at(i).password;
			std::string name = config::getInstance().nodeList()->at(i).name;

			std::cout << "Connecting to " << ip << " ..." << std::endl;
			ssh* s = new ssh(ip, user, password);
			s->_name = name;
			s->connect();
			_sshs->push_back(s);
		}

		refresh();
	}
	
	void waitSetPid(unsigned int count)
	{
		_setPidCount = count;
		piwolfclock now = std::chrono::high_resolution_clock::now();
		while (utils::lastLess(now, 5000) && _setPidCount > 0)
		{
			refresh();
			std::this_thread::sleep_for(std::chrono::milliseconds(200));
		}
	}

	void waitUnsetPid(unsigned int count)
	{
		_unsetPidCount = count;
		piwolfclock now = std::chrono::high_resolution_clock::now();
		while (utils::lastLess(now, 5000) && _unsetPidCount > 0)
		{
			refresh();
			std::this_thread::sleep_for(std::chrono::milliseconds(200));
		}
	}

	unsigned int runningKernels()
	{
		unsigned int ret = 0;
		for (unsigned int i = 0; i < _sshs->size(); i++)
			ret += _sshs->at(i)->runningKernels();

		return ret;
	}
	
	unsigned int coresAvailable()
	{
		unsigned int ret = 0;
		for (unsigned int i = 0; i < _sshs->size(); i++)
			ret += _sshs->at(i)->coresAvailable();

		return ret;
	}
};