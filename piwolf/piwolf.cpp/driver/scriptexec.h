#pragma once

#include <iostream>

//script execution
struct scriptexec
{
public:
	unsigned int pid;
	std::string script;
	std::string connectedSessions;
	
	scriptexec(unsigned int _pid, std::string& _script, std::string _connectedSessions = "")
	{
		pid = _pid;
		script = _script;
		connectedSessions = _connectedSessions;
	}
};