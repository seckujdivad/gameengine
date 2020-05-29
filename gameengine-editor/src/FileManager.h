#pragma once

#include <nlohmann/json.hpp>

#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <stack>

const int FC_MODE_INSERT = 1;
const int FC_MODE_RENAME = 2;
const int FC_MODE_ARRAY_REMOVE = 3;
const int FC_MODE_ARRAY_APPEND = 4;

struct FileChange
{
	std::vector<std::string> key_path;
	nlohmann::json data;
	int mode = FC_MODE_INSERT;
};

class FileManager
{
private:
	std::vector<FileChange> m_file_changes;
	std::string m_file_path = "";

public:
	FileManager();
	~FileManager();

	void QueueChange(FileChange change);
	void SetPath(std::string path);
	void WriteChanges();
	void ClearChanges();
};