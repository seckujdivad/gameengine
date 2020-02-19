#pragma once

#include <nlohmann/json.hpp>

#include <wx/wxprec.h>

#include <string>
#include <vector>
#include <map>

#include "GLComponents.h"
#include "Scene.h"
#include "Model.h"
#include "Camera.h"
#include "render/ShaderProgram.h"
#include "loaders/models/PlyLoader.h"

using nlohmann::json;

Scene* InitialiseScene(std::string path, std::string filename);