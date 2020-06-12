#include "FileManager.h"

FileManager::FileManager()
{
	
}

FileManager::~FileManager()
{
	this->WriteData();
}

void FileManager::SetPath(std::string path)
{
	this->m_file_path = path;
	this->LoadData();
}

nlohmann::json& FileManager::GetData()
{
	if (this->m_file_loaded)
	{
		return this->m_contents;
	}
	else
	{
		throw std::runtime_error("Can't fetch data as no file is loaded");
	}
}

void FileManager::WriteData()
{
	std::string output_preprocess = this->m_contents.dump(2);
	std::string output_postprocess;
	char output_char;
	std::stack<int> state;

	int IN_STR = 0;
	int IN_LIST = 1;
	int IN_OBJ = 2;

	bool allow_char = true;
	int num_spaces = 1;

	for (size_t i = 0; i < output_preprocess.size(); i++)
	{
		output_char = output_preprocess.at(i);
		allow_char = true;

		if ((state.size() > 0) && (state.top() == IN_STR))
		{
			if (output_char == '"')
			{
				state.pop();
			}
		}
		else if (output_char == '"')
		{
			state.push(IN_STR);
		}
		else if (output_char == '[')
		{
			state.push(IN_LIST);
		}
		else if (output_char == '{')
		{
			state.push(IN_OBJ);
		}
		else if ((output_char == ']') || (output_char == '}'))
		{
			state.pop();
		}
		else if ((state.size() > 0) && (state.top() == IN_LIST) && ((output_char == '\n') || (output_char == '\r') || (output_char == ' ')))
		{
			allow_char = false;
			if (output_char == ' ')
			{
				if (num_spaces == 0)
				{
					allow_char = true;
				}
				num_spaces++;
			}
		}
		else if ((state.size() > 0) && (state.top() == IN_LIST) && (output_char == ','))
		{
			num_spaces = 0;
		}

		if (allow_char)
		{
			output_postprocess.push_back(output_char);
		}
	}

	std::ofstream output_file;
	output_file.open(this->m_file_path, std::ios::trunc);
	output_file << output_postprocess;
	output_file.close();
}

void FileManager::LoadData()
{
	if (this->m_file_path != "")
	{
		std::ifstream file;
		file.open(this->m_file_path);

		std::string file_contents;
		if (file.is_open())
		{
			std::string line;
			while (std::getline(file, line))
			{
				file_contents = file_contents + line + '\n';
			}
		}
		else
		{
			throw std::runtime_error("Can't open file " + this->m_file_path);
		}
		file.close();

		this->m_contents = nlohmann::json::parse(file_contents);
	}

	this->m_file_loaded = true;
}