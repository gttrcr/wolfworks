#pragma once

#include <iostream>
#include <functional>

#include "../core/console.h"

#define ARGS [=](const std::string& strArg, const unsigned int uintArg)

struct menu;
struct choice
{
public:
	choice(std::string title, console::keys key, std::function<void(void)> consFunc, std::function<void(const std::string&, const unsigned int)> taskFunc, menu* childMenu = nullptr)
	{
		_title = title;
		_key = key;
		_consFunc = consFunc;
		_taskFunc = taskFunc;
		_childMenu = childMenu;
	}

	void execConsFunc()
	{
		_consFunc();
	}
	
	void execTaskFunc(const std::string& strArg = "", const unsigned int uintArg = 0)
	{
		_taskFunc(strArg, uintArg);
	}

	console::keys key()
	{
		return _key;
	}
	
	std::string& title()
	{
		return _title;
	}
	
	menu* childMenu()
	{
		return _childMenu;
	}

private:
	std::string _title;
	console::keys _key;
	std::function<void(void)> _consFunc;
	std::function<void(const std::string&, const unsigned int)> _taskFunc;
	menu* _childMenu;
};

struct menu
{
private:
	std::vector<choice> _choices;

public:
	void addChoice(choice it)
	{
		_choices.push_back(it);
	}

	unsigned int size()
	{
		return _choices.size();
	}

	choice* at(unsigned int i)
	{
		return &_choices[i];
	}
};