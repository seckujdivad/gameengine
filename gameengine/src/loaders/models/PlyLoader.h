#pragma once

#include <wx/wxprec.h>

#include <vector>
#include <string>
#include <fstream>

#include "../../Model.h"

struct PlyType
{
	bool is_list;
	bool is_ints;
};

struct PlyElement
{
	std::string name;
	int num_elements;
	std::vector<PlyType> types;
	std::vector<std::string> field_names;
};

struct PlyValueList
{
	bool is_ints;
	std::vector<double> values_double;
	std::vector<int> values_ints;
};

Model* ModelFromPly(std::string path);
bool IsPlyInt(std::string type_name);

std::vector<std::string> SplitOnChar(std::string string, char splitter, bool add_empty = false);
std::vector<std::string> SplitOnChar(std::string string, std::string splitter, bool add_empty = false);

template<typename T>
int FindInVector(std::vector<T> to_search, T search_item);