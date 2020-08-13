#pragma once

#include <string>
#include <iostream>
#include <filesystem>

#include <nlohmann/json.hpp>

#include "models/PlyLoader.h"
#include "../scene/Scene.h"

Scene* SceneFromJSON(std::filesystem::path root_path, std::filesystem::path file_name);
Scene* SceneFromJSON(std::string root_path, std::string file_name);