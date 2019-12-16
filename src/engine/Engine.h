#pragma once

#include <nlohmann/json.hpp>

#include <string>
#include <fstream>
#include <iostream>
#include <vector>

#include "Scene.h"
#include "Model.h"
#include "Camera.h"

using nlohmann::json;

Scene* InitialiseScene(std::string path);