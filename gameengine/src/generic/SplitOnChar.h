#pragma once

#include <vector>
#include <string>

std::vector<std::string> SplitOnChar(const std::string& string, char splitter);
std::vector<std::string> SplitOnString(const std::string& string, std::string splitter);

std::vector<std::string> SplitOnLineEnd(const std::string& string);

enum class LineEnding
{
	None, //no line endings detected at all
	CRLF, //windows style
	LF //unix style
};

LineEnding GetLineEnding(const std::string& string);

bool StartsWith(const std::string& string, const std::string& substring);