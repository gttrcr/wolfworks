#pragma once

#include <iostream>
#include <vector>

//piwolfparallelformat
struct pwpf
{
public:
	struct computationVar
	{
	public:
		std::string name;
		std::string start;
		std::string stop;
		std::string estimation;
	};

	struct testingVar
	{
	public:
		std::string name;
		std::vector<std::string> values;
		std::string from;
		std::string to;

		bool operator==(const pwpf::testingVar& tv)
		{
			return name == tv.name
				&& std::equal(values.begin(), values.end(), tv.values.begin())
				&& from == tv.from
				&& to == tv.to;
		}
	};

	struct computation
	{
	public:
		std::string script;
		computationVar vars;
	};

	struct testing
	{
	public:
		std::string script;
		testingVar vars;
	};

	bool stringToObj(const std::string& content)
	{
		try
		{
			json pwpfjson = json::parse(content);

			//computation section
			comp.script = pwpfjson["computation"]["script"];
			comp.vars.name = pwpfjson["computation"]["vars"]["name"];
			comp.vars.start = pwpfjson["computation"]["vars"]["start"];
			comp.vars.stop = pwpfjson["computation"]["vars"]["stop"];
			json tmpjson = pwpfjson["computation"]["vars"];
			if (tmpjson.find("estimation") != tmpjson.end())
				comp.vars.estimation = tmpjson["estimation"];

			//testing section
			test.script = pwpfjson["testing"]["script"];
			test.vars.name = pwpfjson["testing"]["vars"]["name"];

			json tValues = pwpfjson["testing"]["vars"]["values"];
			for (auto tvit = tValues.begin(); tvit != tValues.end(); tvit++)
				test.vars.values.push_back(tvit.value());

			test.vars.from = pwpfjson["testing"]["vars"]["from"];
			test.vars.to = pwpfjson["testing"]["vars"]["to"];

			return true;
		}
		catch (std::exception& ex)
		{
			return false;
		}
	}
	
	json objToJson()
	{
		json jsonObj;

		//computation part
		jsonObj["computation"]["script"] = comp.script;
		jsonObj["computation"]["vars"]["name"] = comp.vars.name;
		jsonObj["computation"]["vars"]["start"] = comp.vars.start;
		jsonObj["computation"]["vars"]["stop"] = comp.vars.stop;
		jsonObj["computation"]["vars"]["estimation"] = comp.vars.estimation;

		//testing part
		jsonObj["testing"]["script"] = test.script;
		jsonObj["testing"]["vars"]["name"] = test.vars.name;
		jsonObj["testing"]["vars"]["values"] = test.vars.values;
		jsonObj["testing"]["vars"]["from"] = test.vars.from;
		jsonObj["testing"]["vars"]["to"] = test.vars.to;

		return jsonObj;
	}

	computation comp;
	testing test;
};