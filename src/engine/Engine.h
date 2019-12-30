#pragma once

#include <nlohmann/json.hpp>

#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <tuple>
#include <regex>

#include "Scene.h"
#include "Model.h"
#include "Camera.h"

using nlohmann::json;

Scene* InitialiseScene(std::string path, std::string filename);

Model* ModelFromPly(std::string path);