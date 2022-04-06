#pragma once

#include "pwpf.h"

//piwolfstoreddata
struct pwsd
{
public:
    struct result
    {
    public:
        std::string name;
        std::vector<std::string> start;
        std::vector<std::string> stop;
    };

    struct estimation
    {
    public:
        std::string script;
        pwpf::testingVar vars;
        result res;
    };

    bool stringToObj(const std::string& content)
    {
        try
        {
            json pwsdjson = json::parse(content);
            json estJson = pwsdjson["estimation"];
            for (auto it = estJson.begin(); it != estJson.end(); it++)
            {
                //get script string
                estimation e;
                e.script = it.value()["script"];

                //get vars
                pwpf::testingVar testV;
                testV.name = it.value()["vars"]["name"];

                json vsJson = it.value()["vars"]["values"];
                std::vector<std::string> vals;
                for (auto vs = vsJson.begin(); vs != vsJson.end(); vs++)
                    vals.push_back(vs.value());

                testV.values = vals;
                testV.from = it.value()["vars"]["from"];
                testV.to = it.value()["vars"]["to"];
                e.vars = testV;

                //get result
                result r;
                r.name = it.value()["result"]["name"];
                json startIntJson = it.value()["result"]["start"];
                for (auto i = startIntJson.begin(); i != startIntJson.end(); i++)
                    r.start.push_back(i.value());

                json stopIntJson = it.value()["result"]["stop"];
                for (auto i = stopIntJson.begin(); i != stopIntJson.end(); i++)
                    r.stop.push_back(i.value());
                e.res = r;

                est.push_back(e);
            }

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
        for (unsigned int i = 0; i < est.size(); i++)
        {
            jsonObj["estimation"][i]["script"] = est[i].script;

            jsonObj["estimation"][i]["vars"]["name"] = est[i].vars.name;
            jsonObj["estimation"][i]["vars"]["values"] = est[i].vars.values;
            jsonObj["estimation"][i]["vars"]["from"] = est[i].vars.from;
            jsonObj["estimation"][i]["vars"]["to"] = est[i].vars.to;

            jsonObj["estimation"][i]["result"]["name"] = est[i].res.name;
            jsonObj["estimation"][i]["result"]["start"] = est[i].res.start;
            jsonObj["estimation"][i]["result"]["stop"] = est[i].res.stop;
        }

        return jsonObj;
    }

    std::vector<estimation> est;
};