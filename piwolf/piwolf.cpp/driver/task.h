#pragma once

#include <mutex>
#include <algorithm>
#include <math.h>
#include <stdexcept>

#include "../core/console.h"
#include "pwpf.h"
#include "scripter.h"
#include "updater.h"

class task
{
private:
	scripter* _scripter;
	updater* _updater;
	std::string _tmpLoadedFile;

	task()
	{

	}

public:
	static task& getInstance()
	{
		static task instance;
		return instance;
	}

	void set(scripter* scripter, updater* updater)
	{
		_scripter = scripter;
		_updater = updater;
	}

	void execution()
	{
		if (_tmpLoadedFile == "")
		{
			std::cout << "There is no script to execute" << std::endl;
			throw std::invalid_argument("");
		}
	}

	void loadScript(const std::string& fileName)
	{
		if (utils::checkFileExists(fileName))
		{
			_tmpLoadedFile = fileName;
			std::cout << "File uploaded successfully" << std::endl;
		}
		else
			std::cout << "File does not exists" << std::endl;
	}

	void singleExecution()
	{
		std::cout << "Selecting the best node ..." << std::endl;
		_updater->refresh();
		if (_updater->_sshs->size() > 0)
		{
			std::vector<ssh*>::iterator it = std::find_if(_updater->_sshs->begin(), _updater->_sshs->end(), [](ssh* s) {return s->availableForComputation(); });
			if (it != _updater->_sshs->end())
			{
				std::cout << "Selected node " << (*it)->name() << " with ip " << (*it)->ip() << std::endl;
				std::cout << "Starting script ..." << std::endl;
				err err;
				if (_scripter->tryAdd(*it, _tmpLoadedFile, err))
				{
					std::cout << "Script started" << std::endl;
					_updater->waitSetPid(1);
				}
				else
					std::cout << "Error on starting script" << std::endl;
			}
			else
				std::cout << "No node available" << std::endl;
		}
		else
			std::cout << "No node available" << std::endl;
	}

	void multipleExecutionOnSomeCores(unsigned int userNumberOfCore)
	{
		std::cout << "Checking if the script is PiWolf format for parallelization ..." << std::endl;
		pwpf pwpfObj;
		if (!pwpfObj.stringToObj(utils::readFile(_tmpLoadedFile)))
			std::cout << _tmpLoadedFile << " is not valid for parallellization" << std::endl;
		else
		{
			std::cout << _tmpLoadedFile << " is valid for parallelization" << std::endl;
			std::cout << "Selecting best nodes ..." << std::endl;
			_updater->refresh();
			unsigned int availableCores = userNumberOfCore;
			std::vector<ssh*> selection;
			for (unsigned int i = 0; i < _updater->_sshs->size(); i++)
			{
				ssh* current = _updater->_sshs->at(i);
				if (current->availableForComputation() && userNumberOfCore > 0)
				{
					userNumberOfCore -= current->coresAvailable();
					selection.push_back(current);
				}
			}
			availableCores -= std::max((int)userNumberOfCore, 0);

			if (availableCores > 0)
			{
				std::cout << std::to_string(availableCores) << " cores available over " << std::to_string(selection.size()) << " nodes" << std::endl;
				std::cout << "Starting time estimation ..." << std::endl;
				err err;
				err_n err_n = _scripter->estimation(selection[0], availableCores, pwpfObj, err);
				err.add(err_n);

				if (err_n == err_n::ESTIMATION_OK)
				{
					std::cout << "Estimation completed" << std::endl;
					std::cout << "Starting script ..." << std::endl;
					if (_scripter->tryAdd(selection, err))
					{
						std::cout << "Script started" << std::endl;
						_updater->waitSetPid(availableCores);
					}
					else
						std::cout << "Error on starting script" << std::endl;
				}
				else
					std::cout << "Error during estimation, error " << err.getStackTrace() << std::endl;
			}
			else
				std::cout << "No cores available" << std::endl;
		}
	}

	void abortAllAndLaunchExecutions()
	{
		for (unsigned int i = 0; i < _updater->_sshs->size(); i++)
		{
			ssh* current = _updater->_sshs->at(i);
			std::cout << "Get running kernels for node " << current->name() << std::endl;
			_updater->refresh();
			unsigned int runningKernels = current->runningKernels();
			std::cout << "There are " << runningKernels << " kernels running" << std::endl;
			std::cout << "Aborting all ..." << std::endl;
			current->abortAll();
			_updater->waitUnsetPid(runningKernels);
			std::cout << "Aborted" << std::endl;
		}

		std::cout << "All kernels aborted" << std::endl;
		_updater->refresh();
		if (_updater->runningKernels() == 0)
		{
			unsigned int coresAvailable = _updater->coresAvailable();
			std::cout << "Launching execution on " << coresAvailable << " cores" << std::endl;
			multipleExecutionOnSomeCores(coresAvailable);
			std::cout << "Lanch completed" << std::endl;
		}
		else
			std::cout << "Error on retreiving running kernels" << std::endl;
	}

	void multipleExecutionOnSomeCoresParallelWolfram(unsigned int userNumberOfCore)
	{

	}

	void abortAllAndLaunchExecutionsParallelWolfram()
	{

	}

	void abortAll()
	{
		std::cout << "Aborting all ..." << std::endl;
		for (unsigned int i = 0; i < _updater->_sshs->size(); i++)
			_updater->_sshs->at(i)->abortAll();
		std::cout << "Aborted all kernels" << std::endl;
	}

	void refreshNodeStatus()
	{
		std::cout << "Refreshing ..." << std::endl;
		_updater->refresh();
		std::cout << "Refresh completed" << std::endl;
	}

	void exitScreen()
	{
		//nothing to do
	}
	
	void realTimeNodeStatus()
	{
		std::cout << "9)\tExit" << std::endl;
		
		unsigned int ipSpace = 20;
		unsigned int nameSpace = 10;
		unsigned int coreNumberSpace = 2;
		unsigned int cpuTempSpace = 5;
		std::cout << console::positionStream(0, 2)
			<< utils::center("user@ip:port", ipSpace)
			<< "|" << utils::center("name", nameSpace)
			<< "|" << utils::center("cN", coreNumberSpace)
			<< "|" << utils::center("cpuT", cpuTempSpace) << std::endl;
		
		while (true)
		{
			std::cout << console::positionStream(0, 1) << std::endl;
			_updater->refresh();
			for (unsigned int i = 0; i < _updater->_sshs->size(); i++)
			{
				ssh* current = _updater->_sshs->at(i);
				std::cout << utils::center(current->user() + "@" + current->ip(), 20)
					<< "|" << utils::center(current->name(), 10)
					<< "|" << utils::center(std::to_string(current->coreNumber()), 2)
					<< "|" << utils::center(current->cpuTemp(), 5) << std::endl;
				
				for (unsigned int k = 0; k < current->pidOfKernels()->size(); k++)
				{
					unsigned int currentPid = current->pidOfKernels()->at(k);
					std::string progrStr = "\t" + std::to_string(currentPid);
					double progress = current->getProgress(currentPid);
					progrStr += " " + std::to_string(progress);
					if (progress > 1)
						progrStr += " (time was underestimated by PiWolf)";
					
					std::cout << progrStr << std::endl;
				}

				std::cout << std::endl;
			}
			
			std::this_thread::sleep_for(std::chrono::milliseconds(1000));
		}
	}

	void exit()
	{
		//implement stop
	}

	void clearFolders()
	{
		std::cout << "Clearing folders ..." << std::endl;
		for (unsigned int i = 0; i < _updater->_sshs->size(); i++)
			_updater->_sshs->at(i)->clearFolders();
		std::cout << "Folder cleared" << std::endl;
	}

	void reboot()
	{
		std::cout << "Sending reboot ..." << std::endl;
		for (unsigned int i = 0; i < _updater->_sshs->size(); i++)
			_updater->_sshs->at(i)->reboot();
		std::cout << "Reboot sent, wait for the restart of nodes" << std::endl;
	}

	void shutdown()
	{
		std::cout << "Sending shutdown ..." << std::endl;
		ssh* I;
		for (unsigned int i = 0; i < _updater->_sshs->size(); i++)
		{
			ssh* current = _updater->_sshs->at(i);
			if (current->ip() == utils::myIp())
				I = current;
			else
				current->shutdown();
		}
		std::cout << "Shutdown sent" << std::endl;
		std::cout << "Goodbye" << std::endl;
		I->shutdown();
	}
};