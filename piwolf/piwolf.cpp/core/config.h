#pragma once

#include <iostream>
#include <vector>

#include "node.h"

class config
{
private:
	std::vector<node>* _nodeList;
	config()
	{
		
	}
	
public:
	static config& getInstance()
	{
		static config instance;
		return instance;
	}
	
	std::string configFile()
	{
		return "config.json";
	}

	std::string storeFile()
	{
		return "store.json";
	}
	
	std::vector<node>* nodeList()
	{
		return _nodeList;
	}
	
	void firstConfiguration(const std::string& configFile)
	{
		std::cout << "This the first configuration process, will be written the file " << configFile << std::endl;
		
		//Insert the number of nodes
		bool inputOk = false;
		std::string input = "";
		unsigned int nodes = 0;
		while (!inputOk)
		{
			std::cout << "How many computational node are there (how many devices do you want to control)? ";
			std::cin >> input;
			if (!utils::stringIsUnsignedInt(&input, &nodes))
				std::cout << input << " is not a valid number for number of nodes" << std::endl;
			else
				inputOk = true;
		}
		
		json configJson;
		for (unsigned int i = 0; i < nodes; i++)
		{
			json configForNode;
		
			//Insert name for node
			std::string nameForNode = "";
			std::cout << "Name for node " << i << " ";
			std::cin >> nameForNode;
			configForNode["name"] = nameForNode;
		
			//Insert ip for node
			std::string ip = "";
			inputOk = false;
			input = "";
			while (!inputOk)
			{
				std::cout << "IP Address of node " << i << " xxx.xxx.xxx.xxx ";
				std::cin >> input;
				bool hasPort;
				std::string ipWithoutPort;
				unsigned int port;
				if (!utils::isValidIpAddress(input, hasPort, ipWithoutPort, port))
					std::cout << input << " is not a valid ip address" << std::endl;
				else
				{
					ip = ipWithoutPort;
					inputOk = true;
				}
			}
			configForNode["ip"] = ip;
		
			//Insert user for node
			std::string userForNode = "";
			std::cout << "User for node " << i << " ";
			std::cin >> userForNode;
			configForNode["user"] = userForNode;
		
			//Insert password for node
			std::string passwordForNode = "";
			std::cout << "Password for node " << i << " ";
			std::cin >> passwordForNode;
			configForNode["password"] = passwordForNode;
		
			configJson[i] = configForNode;
		}
		
		utils::writeFile(configFile, configJson.dump(4));
		loadConfiguration(configFile);
	}
	
	void loadConfiguration(const std::string& configFile)
	{
		_nodeList = new std::vector<node>();
		std::string jsonString = utils::readFile(configFile);
		json jsonConfig = json::parse(jsonString);
		
		for (auto it = jsonConfig.begin(); it != jsonConfig.end(); ++it)
		{
			json nodeConfigJson = *it;
			node n;
			n.name = nodeConfigJson["name"];
			n.ip = nodeConfigJson["ip"];
			n.user = nodeConfigJson["user"];
			n.password = nodeConfigJson["password"];
			_nodeList->push_back(n);
		}
	}
};