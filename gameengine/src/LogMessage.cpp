#include "LogMessage.h"

#include <fstream>
#include <vector>
#include <chrono>
#include <ctime>

void LogMessage(std::string message, bool show_time)
{
	std::ofstream output_file;
	output_file.open(GAMEENGINE_LOG_PATH, std::ios_base::app);

	std::string padding;
	if (show_time)
	{
		std::chrono::system_clock::time_point now_time_point = std::chrono::system_clock::now();
		std::time_t now_time_t = std::chrono::system_clock::to_time_t(now_time_point);
		std::string now_string = std::ctime(&now_time_t);
		now_string = now_string.substr(0, now_string.size() - 1); //remove the newline that is added for some reason

		output_file << now_string << ": ";

		for (std::size_t i = 0; i < now_string.size() + 2U; i++)
		{
			padding += ' ';
		}
	}

	std::vector<std::string> lines = { "" };
	for (char& character : message)
	{
		if (character == '\n')
		{
			if (!lines.back().empty())
			{
				lines.push_back("");
			}
		}
		else
		{
			lines.back() += character;
		}
	}

	for (std::size_t i = 0U; i < lines.size(); i++)
	{
		std::string& line = lines.at(i);

		if (!(
			i != 0U
			&& i + 1U == lines.size()
			&& line.empty()
			))
		{
			if (i != 0U)
			{
				output_file << padding;
			}

			output_file << line << std::endl;
		}
	}

	output_file.close();
}

void LogMessage(const char* message, bool show_time)
{
	LogMessage(std::string(message), show_time);
}
