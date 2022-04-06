#pragma once

#include <libssh/libssh.h>
#include <libssh/sftp.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <mutex>

#include "throttled.h"
#include "cmds.h"
#include "error.h"

class ssh
{
	friend class updater;
	
private:
	std::mutex commandMtx;
	std::mutex trasferMtx;
	ssh_session _session;
	bool _isConnected;
	std::string _ip;
	std::string _user;
	std::string _password;
	std::string _name;
	unsigned int _port;
	unsigned int _coreNumber;
	std::vector<double>* _cpuLoad;
	double _ramLoad;
	std::string _cpuTemp;
	throttled* _throttled;
	
	std::vector<unsigned int>* _pidOfKernels;
	std::vector<unsigned int>* _lastPidOfKernels;
	
	std::map<unsigned int, double>* _progress;

	void setProgress(unsigned int uint, double d)
	{
		std::map<unsigned int, double>::iterator it = _progress->find(uint);
		if (it == _progress->end())
			_progress->insert(std::pair<unsigned int, double>(uint, d));
		else
			it->second = d;
	}
	
public:
	ssh(std::string ip, std::string user, std::string password, unsigned int port = 22)
	{
		_isConnected = false;
		_ip = ip;
		_user = user;
		_password = password;
		_name = "";
		_port = port;
		_coreNumber = 0;
		_cpuLoad = nullptr;
		_ramLoad = 0;
		_cpuTemp = "";
		_throttled = new throttled();
		_pidOfKernels = new std::vector<unsigned int>();
		_lastPidOfKernels = new std::vector<unsigned int>();
		_progress = new std::map<unsigned int, double>();
	}
	
	~ssh()
	{
		delete _cpuLoad;
		delete _throttled;
		delete _pidOfKernels;
		delete _lastPidOfKernels;
		ssh_disconnect(_session);
		ssh_free(_session);
	}
	
	err_n connect()
	{
		_session = ssh_new();
		if (_session == NULL)
			return err_n::ERROR_CREATING_SESSIONS;

		ssh_options_set(_session, SSH_OPTIONS_HOST, _ip.c_str());
		ssh_options_set(_session, SSH_OPTIONS_PORT, &_port);

		// Connect to server
		int rc = ssh_connect(_session);
		if (rc != SSH_OK)
		{
			ssh_free(_session);
			return err_n::ERROR_CONNECTING;
		}

		// Authenticate ourselves
		rc = ssh_userauth_password(_session, NULL, _password.c_str());
		if (rc != SSH_AUTH_SUCCESS)
		{
			ssh_disconnect(_session);
			ssh_free(_session);
			
			return err_n::ERROR_AUTHENTICATING;
		}

		_isConnected = true;

		return err_n::CONNECTION_OK;
	}
	
	std::string ip()
	{
		return _ip;
	}
	
	std::string user()
	{
		return _user;
	}
	
	std::string name()
	{
		return _name;
	}
	
	unsigned int port()
	{
		return _port;
	}
	
	unsigned int coreNumber()
	{
		return _coreNumber;
	}
	
	std::vector<double>* cpuLoad()
	{
		return _cpuLoad;
	}
	
	double ramLoad()
	{
		return _ramLoad;
	}
	
	std::string cpuTemp()
	{
		return _cpuTemp;
	}
	
	std::vector<unsigned int>* pidOfKernels()
	{
		return _pidOfKernels;
	}
	
	unsigned int coresAvailable()
	{
		return _coreNumber - _pidOfKernels->size();
	}

	unsigned int runningKernels()
	{
		return _pidOfKernels->size();
	}
	
	double getProgress(unsigned int pid)
	{
		std::map<unsigned int, double>::iterator it = _progress->find(pid);
		if (it != _progress->end())
			return it->second;

		return 0;
	}
	
	bool availableForComputation()
	{
		return (_pidOfKernels->size() < _coreNumber) && _isConnected;
	}
	
	err_n remoteCommand(const std::string& cmd, std::string& result)
	{
		commandMtx.lock();
		
		ssh_channel channel;
		channel = ssh_channel_new(_session);
		if (channel == NULL)
		{
			commandMtx.unlock();
			return err_n::ERROR_NEW_SSH_CHANNEL;
		}

		int rc = ssh_channel_open_session(channel);
		if (rc != SSH_OK)
		{
			ssh_channel_free(channel);
			commandMtx.unlock();
			return err_n::ERROR_CHANNEL_OPEN_SESSION;
		}

		rc = ssh_channel_request_exec(channel, cmd.c_str());
		if (rc != SSH_OK)
		{
			ssh_channel_close(channel);
			ssh_channel_free(channel);
			commandMtx.unlock();
			return ERROR_CHANNEL_REQUEST_EXEC;
		}

		result = "";
		char buffer[100000];
		int nbytes = ssh_channel_read(channel, buffer, sizeof(buffer), 0);
		while (nbytes > 0)
		{
			for (int i = 0; i < nbytes; i++)
				result += buffer[i];
			nbytes = ssh_channel_read(channel, buffer, sizeof(buffer), 0);
		}

		ssh_channel_send_eof(channel);
		ssh_channel_close(channel);
		ssh_channel_free(channel);
		
		commandMtx.unlock();
		
		return REMOTE_COMMAND_OK;
	}
	
	err_n transferFile(const std::string& remotePath, const std::string& content)
	{
		trasferMtx.lock();

		sftp_session sftp = sftp_new(_session);
		if (sftp == NULL)
		{
			trasferMtx.unlock();
			return err_n::ERROR_ALLOCATING_SFTP;
		}

		int rc = sftp_init(sftp);
		if (rc != SSH_OK)
		{
			sftp_free(sftp);
			trasferMtx.unlock();
			return err_n::ERROR_INITIALIZING_SFTP;
		}

		int access_type = O_WRONLY | O_CREAT | O_TRUNC;
		int length = strlen(content.c_str());
		sftp_file file = sftp_open(sftp, remotePath.c_str(), access_type, S_IRWXU);
		if (file == NULL)
		{
			sftp_free(sftp);
			trasferMtx.unlock();
			return err_n::ERROR_OPENING_FILE_FOR_WRITING;
		}

		int nwritten = sftp_write(file, content.c_str(), length);
		if (nwritten != length)
		{
			sftp_close(file);
			trasferMtx.unlock();
			return err_n::ERROR_WRITING_DATA_TO_FILE;
		}

		rc = sftp_close(file);
		if (rc != SSH_OK)
		{
			sftp_free(sftp);
			trasferMtx.unlock();
			return err_n::ERROR_CLOSING_WRITTEN_FILE;
		}

		sftp_free(sftp);

		trasferMtx.unlock();

		return err_n::SFTP_OK;
	}

	bool abortAll()
	{
		std::string res;
		return remoteCommand(CMD_ABORT_ALL, res);
	}

	bool clearFolders()
	{
		std::string res;
		std::string command = utils::replaceStringWithString(CMD_CLEAR_FOLDERS, PLACEHOLDER0, utils::getWorkingDir());
		return remoteCommand(command, res);
	}

	bool reboot()
	{
		std::string res;
		return remoteCommand(CMD_REBOOT, res);
	}

	bool shutdown()
	{
		std::string res;
		return remoteCommand(CMD_SHUTDOWN, res);
	}
};