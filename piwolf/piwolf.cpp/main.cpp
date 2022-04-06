#include "core/utils.h"
#include "core/config.h"
#include "driver/scripter.h"
#include "driver/updater.h"
#include "usr/consoler.h"

//manage the script execution
scripter* s;

//update the status of all connections
updater* up;

int main(int argc, char* argv[])
{
	if (!utils::checkFileExists(config::getInstance().configFile()))
		config::getInstance().firstConfiguration(config::getInstance().configFile());
	else
		config::getInstance().loadConfiguration(config::getInstance().configFile());

	s = new scripter();
	up = new updater(s);
	task::getInstance().set(s, up);

	consoler* cl = new consoler(s, up);
	if (argc > 1)
		cl->startCLI(argc, argv);
	else
		cl->start();
}