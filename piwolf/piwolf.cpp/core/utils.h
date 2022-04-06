#pragma once

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/select.h>
#include <termios.h>
#include <unistd.h>
#include <thread>
//#include <json.hpp>
#include "../json/single_include/nlohmann/json.hpp"

#define TRIM_CHARS " \n\r\t\f\v"
using piwolfclock = std::chrono::time_point<std::chrono::high_resolution_clock>;
using json = nlohmann::json;

namespace utils
{
	static inline bool checkFileExists(const std::string& pathAndFileName)
	{
		std::ifstream f(pathAndFileName.c_str());
		return f.good();
	}
	
	static inline void writeFile(const std::string& fileName, const std::string& content)
	{
		std::ofstream ofs(fileName, std::ofstream::trunc);
		ofs << content;
		ofs.close();
	}

	static inline void removeFile(const std::string& fileName)
	{
		std::remove(fileName.c_str());
	}

	static inline std::string readFile(const std::string& fileName)
	{
		std::ifstream t(fileName);
		std::string str((std::istreambuf_iterator<char>(t)), std::istreambuf_iterator<char>());
		return str;
	}
	
	static inline bool stringIsUnsignedInt(std::string* str, unsigned int* number)
	{
		std::string::const_iterator it = str->begin();
		while (it != str->end() && std::isdigit(*it))
			++it;
		if (!str->empty() && it == str->end())
		{
			std::stringstream ss(*str);
			int x = 0;
			ss >> x;
			if (x > 0)
			{
				*number = x;
				return true;
			}

			return false;
		}

		return false;
	}
	
	static inline std::vector<std::string> splitStringByChar(const std::string& str, const char& ch = '\n', const bool removeEmptyEntries = false)
	{
		std::stringstream test(str);
		std::string segment;
		std::vector<std::string> seglist;

		while (std::getline(test, segment, ch))
			if (!(removeEmptyEntries && segment == ""))
				seglist.push_back(segment);

		return seglist;
	}

	static inline bool isValidIpAddress(const std::string& str, bool& hasPort, std::string& ipWithoutPort, unsigned int& port)
	{
		std::vector<std::string> ipPort = splitStringByChar(str, ':');
		hasPort = ipPort.size() == 2;
		ipWithoutPort = ipPort[0];
		if (hasPort)
			port = std::stoi(ipPort[1]);

		std::vector<std::string> ip = splitStringByChar(ipPort[0], '.');
		if (ip.size() == 4)
		{
			bool ok = true;
			for (unsigned int i = 0; i < 4; i++)
				ok &= ip[i].length() != 0;

			return ok;
		}

		return false;
	}
	
	static inline bool stringEndsWithString(std::string& value, const std::string& ending)
	{
		if (ending.size() > value.size())
			return false;
		
		return std::equal(ending.rbegin(), ending.rend(), value.rbegin());
	}
	
	static inline std::string replaceStringWithString(std::string originalString, const std::string& stringToReplace, const std::string& replacementString = "")
	{
		for (unsigned int i = 0; i < originalString.length(); i++)
		{
			if (i + stringToReplace.length() <= originalString.length())
				if (originalString.substr(i, stringToReplace.length()) == stringToReplace)
				{
					originalString.replace(originalString.begin() + i, originalString.begin() + i + stringToReplace.length(), replacementString);
					i += (unsigned int)(replacementString.length() - stringToReplace.length());
				}
		}

		return originalString;
	}

	struct termios orig_termios;

	static inline void disableConioTerminalMode()
	{
		tcsetattr(0, TCSANOW, &orig_termios);
	}

	static inline void setConioTerminalMode()
	{
		struct termios new_termios;

		/* take two copies - one for now, one for later */
		tcgetattr(0, &orig_termios);
		memcpy(&new_termios, &orig_termios, sizeof(new_termios));

		/* register cleanup handler, and set the new terminal mode */
		atexit(disableConioTerminalMode);
		cfmakeraw(&new_termios);
		tcsetattr(0, TCSANOW, &new_termios);
	}

	//static inline int kbhit()
	//{
	//	struct timeval tv = { 0L, 0L };
	//	fd_set fds;
	//	FD_ZERO(&fds);
	//	FD_SET(0, &fds);
	//	return select(1, &fds, NULL, NULL, &tv);
	//}

	static inline unsigned int getch()//bool blocking = false)
	{
		//if (blocking)
		//	while (!kbhit())
		//		std::this_thread::sleep_for(std::chrono::milliseconds(10));
		
		int r;
		unsigned char c;
		if ((r = read(0, &c, sizeof(c))) < 0)
			return r;
		else
			return c;
	}
	
	static inline std::string getWorkingDir()
	{
		char buff[FILENAME_MAX];
		getcwd(buff, FILENAME_MAX);
		std::string ret(buff);
		return ret + "/";
	}
	
	static inline std::string timeStamp()
	{
		const auto p1 = std::chrono::high_resolution_clock::now();
		return std::to_string(std::chrono::duration_cast<std::chrono::seconds>(p1.time_since_epoch()).count());
	}
	
	static inline std::string padRight(const std::string& str, const size_t s, const char& ch = ' ')
	{
		if (str.size() < s)
			return str + std::string(s - str.size(), ch);
		else
			return str;
	}

	static inline std::string padLeft(const std::string& str, const size_t s, const char& ch = ' ')
	{
		if (str.size() < s)
			return std::string(s - str.size(), ch) + str;
		else
			return str;
	}
	
	static inline std::string center(const std::string& str, const size_t s, const char& ch = ' ')
	{
		if (str.size() < s)
		{
			std::string tmp((s - str.size()) / 2, ch);
			return tmp + str + std::string(s - tmp.size() - str.size(), ch);
		}
		return str;
	}

	static inline std::string ltrim(const std::string& s)
	{
		size_t start = s.find_first_not_of(TRIM_CHARS);
		return (start == std::string::npos) ? "" : s.substr(start);
	}

	static inline std::string rtrim(const std::string& s)
	{
		size_t end = s.find_last_not_of(TRIM_CHARS);
		return (end == std::string::npos) ? "" : s.substr(0, end + 1);
	}

	static inline std::string trim(const std::string& s)
	{
		return rtrim(ltrim(s));
	}
	
	static inline bool lastLess(piwolfclock clock, unsigned int msDuration)
	{
		auto millis = std::chrono::duration_cast<std::chrono::milliseconds>(clock - std::chrono::high_resolution_clock::now());
		if (millis.count() > msDuration)
			return false;		
		return true;
	}
	
	static inline bool isNumber(const std::string& s)
	{
		std::string::const_iterator it = s.begin();
		while (it != s.end() && std::isdigit(*it)) ++it;
		return !s.empty() && it == s.end();
	}
	
	static inline std::string myIp()
	{
		return "";
	}
}