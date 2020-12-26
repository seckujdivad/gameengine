#include "SplitOnChar.h"

#include <stdexcept>

std::vector<std::string> SplitOnChar(std::string string, char splitter)
{
	std::vector<std::string> result;
	size_t prev_slice = 0;

	for (size_t i = 0; i < string.size(); i++)
	{
		if (string.at(i) == splitter)
		{
			std::string current_slice = string.substr(prev_slice, i - prev_slice);
			if (current_slice != "")
			{
				result.push_back(current_slice);
			}
			prev_slice = i + 1;
		}
	}

	if (prev_slice != string.size())
	{
		std::string current_slice = string.substr(prev_slice, string.size() - prev_slice);
		if (current_slice != "")
		{
			result.push_back(current_slice);
		}
	}

	return result;
}

std::vector<std::string> SplitOnChar(std::string string, std::string splitter)
{
	if (splitter.size() == 1)
	{
		return SplitOnChar(string, splitter.at(0));
	}
	else
	{
		throw std::invalid_argument("Splitter must have length 1");
	}
}