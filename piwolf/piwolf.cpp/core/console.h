#pragma once

#include "utils.h"

namespace console
{
	enum colorCode
	{
		FG_RED     = 31,
		FG_GREEN   = 32,
		FG_BLUE    = 34,
		FG_DEFAULT = 39,
		BG_RED     = 41,
		BG_GREEN   = 42,
		BG_BLUE    = 44,
		BG_DEFAULT = 49
	};

	class colorCodeStream
	{
	private:
		colorCode _cc;

	public:
		colorCodeStream(colorCode cc)
			: _cc(cc) {}

		friend std::ostream& operator<<(std::ostream& os, const colorCodeStream& ccs)
		{
			return os << "\033[" << ccs._cc << "m";
		}
	};

	class positionStream
	{
	private:
		unsigned int _x;
		unsigned int _y;

	public:
		positionStream(unsigned int x, unsigned int y)
			: _x(x)
			, _y(y) {}

		friend std::ostream& operator<<(std::ostream& os, const positionStream& ps)
		{
			return os << "\033[" + std::to_string(ps._y) + ";" + std::to_string(ps._x) + "H";
		}
	};

	enum keys
	{
		null = 0,
		_1   = 49,
		_2,
		_3,
		_4,
		_5,
		_6,
		_7,
		_8,
		_9,
	};

	static keys getKey(bool blocking = false)
	{
		utils::setConioTerminalMode();
		keys ret;

		do {
			switch (utils::getch())
			{
			case keys::_1:
				ret = keys::_1;
				blocking = false;
				break;
			case keys::_2:
				ret = keys::_2;
				blocking = false;
				break;
			case keys::_3:
				ret = keys::_3;
				blocking = false;
				break;
			case keys::_4:
				ret = keys::_4;
				blocking = false;
				break;
			case keys::_5:
				ret = keys::_5;
				blocking = false;
				break;
			case keys::_6:
				ret = keys::_6;
				blocking = false;
				break;
			case keys::_7:
				ret = keys::_7;
				blocking = false;
				break;
			case keys::_8:
				ret = keys::_8;
				blocking = false;
				break;
			case keys::_9:
				ret = keys::_9;
				blocking = false;
				break;
			}
		} while (blocking);

		utils::disableConioTerminalMode();

		return ret;
	}
}