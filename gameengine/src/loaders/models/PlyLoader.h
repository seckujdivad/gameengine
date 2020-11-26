#pragma once

#include <vector>
#include <string>
#include <map>

#include "../../scene/model/Model.h"

ModelGeometry ModelFromPly(std::string path);
bool IsPlyInt(std::string type_name);

std::vector<std::string> SplitOnChar(std::string string, char splitter);
std::vector<std::string> SplitOnChar(std::string string, std::string splitter);