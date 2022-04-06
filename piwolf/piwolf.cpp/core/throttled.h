#pragma once

struct throttled
{
	friend class updater;
	
public:
	void throttledValue(std::string throttledValue)
	{
		_throttledValue = throttledValue;
		
		unsigned int th = std::stoi(_throttledValue);
		_underVoltageDetected = (th & (1 << 0)) == 1;
		_armFrequencyCapped = (th & (1 << 1)) == 1;
		_currentlyThrottled = (th & (1 << 2)) == 1;
		_softTemperatureLimitActive = (th & (1 << 3)) == 1;
		_underVoltageHasOccurred = (th & (1 << 16)) == 1;
		_armFrequencyCappingHasOccurred = (th & (1 << 17)) == 1;
		_throttlingHasOccurred = (th & (1 << 18)) == 1;
		_softTemperatureLimitHasOccurred = (th & (1 << 19)) == 1;
	}
	
	std::string throttledValue()
	{
		return _throttledValue;
	}

private:
	std::string _throttledValue;
	bool _underVoltageDetected;
	bool _armFrequencyCapped;
	bool _currentlyThrottled;
	bool _softTemperatureLimitActive;
	bool _underVoltageHasOccurred;
	bool _armFrequencyCappingHasOccurred;
	bool _throttlingHasOccurred;
	bool _softTemperatureLimitHasOccurred;

//protected:
//	bool underVoltageDetected()
//	{
//		return _underVoltageDetected;
//	}
//	
//	bool armFrequencyCapped()
//	{
//		return _armFrequencyCapped;
//	}
//	
//	bool currentlyThrottled()
//	{
//		return _currentlyThrottled;
//	}
//	
//	bool softTemperatureLimitActive()
//	{
//		return _softTemperatureLimitActive;
//	}
//	
//	bool underVoltageHasOccurred()
//	{
//		return _underVoltageHasOccurred;
//	}
//	
//	bool armFrequencyCappingHasOccurred()
//	{
//		return _armFrequencyCappingHasOccurred;
//	}
//	
//	bool throttlingHasOccurred()
//	{
//		return _throttlingHasOccurred;
//	}
//	
//	bool softTemperatureLimitHasOccurred()
//	{
//		return _softTemperatureLimitHasOccurred;
//	}
};