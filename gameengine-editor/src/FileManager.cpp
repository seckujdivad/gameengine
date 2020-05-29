#include "FileManager.h"

FileManager::FileManager()
{
	
}

FileManager::~FileManager()
{
	this->WriteChanges();
}

void FileManager::QueueChange(FileChange change)
{
	this->m_file_changes.push_back(change);
}

void FileManager::SetPath(std::string path)
{
	this->m_file_path = path;
}

void FileManager::WriteChanges()
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

		nlohmann::json config = nlohmann::json::parse(file_contents);

		FileChange change;
		nlohmann::json item;
		std::vector<nlohmann::json> items;
		for (size_t i = 0; i < this->m_file_changes.size(); i++)
		{
			change = this->m_file_changes.at(i);

			item = config;
			items.clear();
			items.push_back(config);

			for (size_t j = 0; j < change.key_path.size() - 1; j++)
			{
				if (item.is_object())
				{
					if (!item.contains(change.key_path.at(j)))
					{
						item[change.key_path.at(j)] = {};
					}
				}
				else
				{
					throw std::runtime_error("Can't access element");
				}
				items.push_back(items.at(items.size() - 1)[change.key_path.at(j)]);
			}
			
			if (change.mode == FC_MODE_INSERT)
			{
				items.at(items.size() - 1)[change.key_path.at(change.key_path.size() - 1)] = change.data;
			}
			else if (change.mode == FC_MODE_RENAME)
			{
				if (change.data.is_string())
				{
					if (change.key_path.at(change.key_path.size() - 1) != change.data.get<std::string>())
					{
						items.at(items.size() - 1)[change.data.get<std::string>()] = items.at(items.size() - 1).at(change.key_path.at(change.key_path.size() - 1));
						items.at(items.size() - 1).erase(change.key_path.at(change.key_path.size() - 1));
					}
				}
				else
				{
					throw std::runtime_error("Rename mode is only valid when data is a string");
				}
			}
			else
			{
				throw std::runtime_error("Unknown file change mode " + change.mode);
			}

			for (size_t j = items.size() - 1; j > 0; j--)
			{
				items.at(j - 1)[change.key_path.at(j - 1)] = items.at(j);
			}
			config = items.at(0);
		}

		std::string output_preprocess = config.dump(2);
		std::string output_postprocess;
		char output_char;
		std::stack<int> state;

		int IN_STR = 0;
		int IN_LIST = 1;
		int IN_OBJ = 2;

		bool allow_char = true;
		int num_spaces = 0;

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
			else
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

		this->ClearChanges();
	}
}

void FileManager::ClearChanges()
{
	this->m_file_changes.clear();
}