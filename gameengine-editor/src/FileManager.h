#pragma once

#include <nlohmann/json.hpp>

#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <stack>
#include <stdexcept>

class FileManager
{
private:
	std::string m_file_path = "";
	nlohmann::json m_contents;
	bool m_file_loaded = false;

public:
	FileManager();
	~FileManager();

	nlohmann::json& GetData();

	void SetPath(std::string path);
	void WriteData();
	void LoadData();
};