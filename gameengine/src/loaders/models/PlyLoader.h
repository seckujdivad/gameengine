#pragma once

#include <wx/wxprec.h>

#include <vector>
#include <string>
#include <fstream>
#include <map>

#include "../../scene/model/Model.h"

struct PlyType
{
	bool is_list;
	bool is_ints;
};

struct PlyElement
{
	std::string name;
	int num_elements = 0;
	std::vector<PlyType> types;
	std::vector<std::string> field_names;
	std::map<std::string, int> field_name_map;
};

struct PlyValueList
{
	bool is_ints;
	std::vector<double> values_double;
	std::vector<int> values_ints;
};

Model* ModelFromPly(std::string path);
bool IsPlyInt(std::string type_name);

std::vector<std::string> SplitOnChar(std::string string, char splitter);
std::vector<std::string> SplitOnChar(std::string string, std::string splitter);