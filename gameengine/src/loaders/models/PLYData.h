#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <variant>

struct Property
{
	enum class Type
	{
		Double,
		Integer
	};

	Property(std::string name, Type type, bool is_list);

	std::string name;
	Type type;
	bool is_list;
};

using ParsedElement = std::unordered_map<std::string, std::variant<double, int, std::vector<double>, std::vector<int>>>;

struct Element
{
	Element(std::string name, int num_occurrences, std::vector<Property> properties);

	std::string name;
	int num_occurrences;
	std::vector<Property> properties;

	
	ParsedElement GetParsed(const std::vector<std::string>& tokens) const;
};

using PLYData = std::unordered_map<std::string, std::vector<ParsedElement>>;
PLYData ParsePLYFile(const std::vector<std::string>& text);