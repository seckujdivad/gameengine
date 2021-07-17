#include "SplitOnChar.h"

#include <stdexcept>

std::vector<std::string> SplitOnChar(const std::string& string, char splitter)
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

std::vector<std::string> SplitOnString(const std::string& string, std::string splitter)
{
	std::vector<std::string> result;

	std::size_t prev_split = 0;
	std::size_t next_split = string.find(splitter, prev_split);
	while (next_split != std::string::npos)
	{
		result.push_back(string.substr(prev_split, next_split - prev_split));

		prev_split = next_split + splitter.size();
		next_split = string.find(splitter, prev_split);
	}
	result.push_back(string.substr(prev_split, std::string::npos));

	return result;
}

std::vector<std::string> SplitOnLineEnd(const std::string& string)
{
	LineEnding line_ending = GetLineEnding(string);
	if (line_ending == LineEnding::None)
	{
		return std::vector({ string });
	}
	else if (line_ending == LineEnding::CRLF)
	{
		return SplitOnString(string, "\r\n");
	}
	else if (line_ending == LineEnding::LF)
	{
		return SplitOnChar(string, '\n');
	}
	else
	{
		throw std::invalid_argument("Unknown line ending");
	}
}

LineEnding GetLineEnding(const std::string& string)
{
	for (const char c : string)
	{
		if (c == '\r') //assume that the line endings will be correctly formed (and that the carriage return is followed by a line feed) - this is not a validator
		{
			return LineEnding::CRLF;
		}
		else if (c == '\n')
		{
			return LineEnding::LF;
		}
	}

	return LineEnding::None;
}
