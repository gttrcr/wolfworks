#pragma once

#define MAX_ERRN 255

enum err_n
{
	EMPTY_ERR = 0,
	
	SEND_AND_START_SYNC_OK         = 10,
	REMOTE_COMMAND_REQUEST_KO      = 20,
	SFTP_KO                        = 30,
	ESTIMATION_OK                  = 40,
	ESTIMATION_KO                  = 50,
	
	ERROR_CONNECTING               = 60,
	ERROR_AUTHENTICATING           = 70,
	ERROR_NEW_SSH_CHANNEL          = 80,
	ERROR_CHANNEL_OPEN_SESSION     = 90,
	ERROR_CHANNEL_REQUEST_EXEC     = 100,
	ERROR_LOCKING_MUTEX            = 110,
	REMOTE_COMMAND_OK              = 120,

	ERROR_ALLOCATING_SFTP          = 130,
	ERROR_INITIALIZING_SFTP        = 140,
	ERROR_OPENING_FILE_FOR_WRITING = 150,
	ERROR_WRITING_DATA_TO_FILE     = 160,
	ERROR_CLOSING_WRITTEN_FILE     = 170,
	SFTP_OK                        = 180,

	ERROR_CREATING_SESSIONS        = 190,
	CONNECTION_OK                  = 200,
};

struct err
{
private:
	err_n err_flag[MAX_ERRN];
	unsigned int index;

public:
	err()
	{
		index = 0;
		
		for (unsigned int i = 0; i < MAX_ERRN; i++)
			err_flag[i] = err_n::EMPTY_ERR;
	}
	
	void add(err_n errN)
	{
		err_flag[index++] = errN;
	}

	std::string getStackTrace()
	{
		std::string stack = "";
		for (unsigned int i = 0; i < MAX_ERRN; i++)
			stack += (err_flag[i] != err_n::EMPTY_ERR ? (std::to_string(err_flag[i]) + " ") : "");

		return stack;
	}
};