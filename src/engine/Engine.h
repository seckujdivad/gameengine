#pragma once

#include <nlohmann/json.hpp>

#include <string>

#include "Scene.h"
#include "Model.h"
#include "Camera.h"

using nlohmann::json;

Scene* InitialiseScene(std::string path);