#include "LoadFile.h"

#include <fstream>

std::string LoadFile(std::filesystem::path path)
{
	std::ifstream file_stream;
	std::string file_contents;
	std::string line_contents;

	bool is_first_line = true;

	file_stream.open(path);
	if (file_stream.is_open())
	{
		while (std::getline(file_stream, line_contents))
		{
			if (is_first_line)
			{
				file_contents = line_contents;
				is_first_line = false;
			}
			else
			{
				file_contents = file_contents + '\n' + line_contents;
			}
		}
	}
	else
	{
		throw std::invalid_argument("Can't open file at " + path.string());
	}
	return file_contents;
}

std::string LoadFile(std::string path)
{
	return LoadFile(std::filesystem::path(path));
}

std::string LoadFile(const char* path)
{
	return LoadFile(std::string(path));
}
