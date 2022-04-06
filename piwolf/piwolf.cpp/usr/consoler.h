#pragma once

#include <sys/ioctl.h>

#include "../driver/task.h"
#include "menu.h"

class consoler
{
private:
	scripter* _scripter;
	updater* _updater;
	unsigned int _lines;
	unsigned int _columns;
	bool _start;
	std::thread* _consolerThread;
	std::mutex printMtx;

	menu* _mainMenu;
	menu* _executionOptions;

	//void outText(const std::string& content, unsigned int x, unsigned int y, console::colorCode fg = console::colorCode::FG_DEFAULT, console::colorCode bg = console::colorCode::BG_DEFAULT, bool newLine = true)
	//{
	//	printMtx.lock();
	//
	//	std::cout << console::colorCodeStream(fg) << console::colorCodeStream(bg) << console::positionStream(x, y + 1) << content;
	//	if (newLine)
	//		std::cout << std::endl;
	//
	//	printMtx.unlock();
	//}

	void outText(const std::string& content, console::colorCode fg = console::colorCode::FG_DEFAULT, console::colorCode bg = console::colorCode::BG_DEFAULT, bool newLine = true)
	{
		printMtx.lock();
	
		std::cout << console::colorCodeStream(fg) << console::colorCodeStream(bg) << content;
		if (newLine)
			std::cout << std::endl;
	
		printMtx.unlock();
	}

	//void line(unsigned int line, std::string content, console::colorCode fg = console::colorCode::FG_DEFAULT, console::colorCode bg = console::colorCode::BG_DEFAULT)
	//{
	//	unsigned int i = _columns - (content.size() % _columns);
	//	for (; i > 0; i--)
	//		content += ' ';
	//	outText(content, 0, line, fg, bg);
	//}
	//
	//std::string centerStringInConsoleLine(const std::string& content)
	//{
	//	std::string ret = "";
	//	for (unsigned int i = 0; i < _columns / 2 - content.length() / 2; i++)
	//		ret += ' ';
	//	ret += content;
	//
	//	return ret;
	//}
	//
	//std::string horizontalLine(const std::string& s = " ")
	//{
	//	std::string ret = "";
	//	for (unsigned int i = 0; i < _columns; i += s.length())
	//		ret += s;
	//
	//	return ret;
	//}

	void consoleSize()
	{
		struct winsize w;
		ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
		_lines = w.ws_row;
		_columns = w.ws_col;
	}

	void cursor(bool visible)
	{
		if (visible)
			fputs("\e[?25h", stdout);
		else
			fputs("\e[?25l", stdout);
	}

	void clearConsole()
	{
		system("clear");
	}

	std::string userInput(const std::string& label = "")
	{
		outText(label, console::colorCode::FG_DEFAULT, console::colorCode::BG_DEFAULT, false);
		std::string ret;
		std::cin >> ret;

		return ret;
	}

	void printMenu(menu* m, const std::string& title)
	{
		clearConsole();

		std::cout << title << std::endl;
		for (unsigned int i = 0; i < m->size(); i++)
			std::cout << m->at(i)->title() << std::endl;
	}

	void execMenu(menu* m, console::keys key)
	{
		for (unsigned int i = 0; i < m->size(); i++)
		{
			choice* currentChoice = m->at(i);
			if (currentChoice->key() == key)
			{
				clearConsole();
				currentChoice->execConsFunc();
				std::this_thread::sleep_for(std::chrono::milliseconds(2000));
				clearConsole();
				printMenu(_mainMenu, "MENU");
			}
		}
	}

	void consolerTask()
	{
		cursor(false);
		clearConsole();
		printMenu(_mainMenu, "MENU");

		while (_start)
		{
			console::keys key = console::getKey(true);
			if (key != console::keys::null)
				execMenu(_mainMenu, key);
		}
	}

	//-----------------------------------------------------

	void malformedCommand()
	{
		std::cout << "Error: malformed command" << std::endl;
	}

	void loadScript()
	{
		std::cout << "Looking in executable folder: " << utils::getWorkingDir() << " ..." << std::endl;
		std::string fileName = userInput("File name to load: ");
		task::getInstance().loadScript(fileName);
	}

	void execution()
	{
		try
		{
			task::getInstance().execution();
			
			std::cout << "Select an option" << std::endl;
			for (unsigned int i = 0; i < _executionOptions->size(); i++)
				std::cout << _executionOptions->at(i)->title() << std::endl;

			console::keys key = console::getKey(true);
			execMenu(_executionOptions, key);
		}
		catch (std::invalid_argument& ex)
		{
			
		}
		catch (std::exception& ex)
		{
			std::cout << "Generic error: " << ex.what() << std::endl;
		}
	}

	void multipleExecutionOnSomeCores()
	{
		std::string numberOfCoreStr = userInput("Number of cores: ");
		int userNumberOfCore = std::stoi(numberOfCoreStr);
		task::getInstance().multipleExecutionOnSomeCores(userNumberOfCore);
	}

	void multipleExecutionOnSomeCoresParallelWolfram()
	{
		std::string numberOfCoreStr = userInput("Number of cores: ");
		int userNumberOfCore = std::stoi(numberOfCoreStr);
		task::getInstance().multipleExecutionOnSomeCoresParallelWolfram(userNumberOfCore);
	}

public:
	consoler(scripter* s, updater* u)
	{
		_scripter = s;
		_updater = u;
		_start = false;

		consoleSize();

		_executionOptions = new menu();
		_executionOptions->addChoice(choice("1)\tSingle execution", console::keys::_1,
			[=]() { task::getInstance().singleExecution(); }, ARGS{ task::getInstance().singleExecution(); }));
		_executionOptions->addChoice(choice("2)\tTry execution on some cores", console::keys::_2,
			[=]() { consoler::multipleExecutionOnSomeCores(); }, ARGS{ task::getInstance().multipleExecutionOnSomeCores(uintArg); }));
		_executionOptions->addChoice(choice("3)\tAbort all and launch executions", console::keys::_3,
			[=]() { task::getInstance().abortAllAndLaunchExecutions(); }, ARGS{ task::getInstance().abortAllAndLaunchExecutions(); }));
		_executionOptions->addChoice(choice("4)\tTry execution on some cores (parallel wolfram)", console::keys::_4,
			[=]() { consoler::multipleExecutionOnSomeCoresParallelWolfram(); }, ARGS{ task::getInstance().multipleExecutionOnSomeCoresParallelWolfram(uintArg); }));
		_executionOptions->addChoice(choice("5)\tAbort all and launch executions (parallel wolfram)", console::keys::_5,
			[=]() { task::getInstance().abortAllAndLaunchExecutionsParallelWolfram(); }, ARGS{ task::getInstance().abortAllAndLaunchExecutionsParallelWolfram(); }));
		_executionOptions->addChoice(choice("6)\tAbort all", console::keys::_6,
			[=]() { task::getInstance().abortAll(); }, ARGS{ task::getInstance().abortAll(); }));
		_executionOptions->addChoice(choice("9)\tExit", console::keys::_9,
			[=]() { task::getInstance().exitScreen(); }, ARGS{ task::getInstance().exitScreen(); }));

		_mainMenu = new menu();
		_mainMenu->addChoice(choice("1)\tLoad script", console::keys::_1,
			[=]() { consoler::loadScript(); }, ARGS{ task::getInstance().loadScript(strArg); }));
		_mainMenu->addChoice(choice("2)\tExecution", console::keys::_2,
			[=]() { consoler::execution(); }, ARGS{ consoler::malformedCommand(); }, _executionOptions));
		_mainMenu->addChoice(choice("3)\tReal time node status", console::keys::_3,
			[=]() { task::getInstance().realTimeNodeStatus(); }, ARGS{ task::getInstance().realTimeNodeStatus(); }));
		_mainMenu->addChoice(choice("4)\t...", console::keys::_4,
			[=]() { }, ARGS{ }));
		_mainMenu->addChoice(choice("5)\tRefresh node status", console::keys::_5,
			[=]() { task::getInstance().refreshNodeStatus(); }, ARGS{ }));
		_mainMenu->addChoice(choice("6)\tClear data foldes and files", console::keys::_6,
			[=]() { task::getInstance().clearFolders(); }, ARGS{ task::getInstance().clearFolders(); }));
		_mainMenu->addChoice(choice("7)\tReboot nodes", console::keys::_7,
			[=]() { task::getInstance().reboot(); }, ARGS{ task::getInstance().reboot(); }));
		_mainMenu->addChoice(choice("8)\tShutdown nodes", console::keys::_8,
			[=]() { task::getInstance().shutdown(); }, ARGS{ task::getInstance().shutdown(); }));
		_mainMenu->addChoice(choice("9)\tExit", console::keys::_9,
			[=]() { task::getInstance().exit(); }, ARGS{ }));
	}

void start()
	{
		_start = true;
		_consolerThread = new std::thread(&consoler::consolerTask, this);
		_consolerThread->join();
	}

	void stop()
	{
		cursor(true);
		_start = false;
	}
	
	void startCLI(unsigned int argc, char* argv[])
	{
		for (unsigned int i = 1; i < argc;)
		{
			std::string cmd = argv[i++];

			if (cmd[0] == '-')
				cmd = utils::replaceStringWithString(cmd, "-");
			else
				break;

			menu* currM = _mainMenu;
			for (unsigned int m = 0; m < cmd.length(); m++)
			{
				//Check if is the last menu
				bool last = false;
				if (m == cmd.length() - 1)
					last = true;

				unsigned int menuIndex = cmd[m] - '0' - 1;
				choice* ch = currM->at(menuIndex);
				if (ch->childMenu() != nullptr && !last)
					currM = ch->childMenu();
				else if (last)
				{
					//If is the last menu, get the list of args
					std::vector<std::string> argV;
					for (unsigned int c = i; c < argc; c++)
					{
						std::string cmd = argv[c];
						if (cmd[0] != '-')
						{
							i++;
							argV.push_back(argv[c]);
						}
						else
							break;
					}

					//Execute the corresponding task function
					if (argV.size() == 0)
						ch->execTaskFunc();
					else if (utils::isNumber(argV[0]))
					{
						if (argV.size() == 2)
							ch->execTaskFunc(argV[1], std::stoi(argV[0]));
						else
							ch->execTaskFunc("", std::stoi(argV[0]));
					}
					else if (argV.size() == 2)
						ch->execTaskFunc(argV[0], std::stoi(argV[1]));
					else
						ch->execTaskFunc(argV[0]);
				}
			}
		}
	}
};