#pragma once

#include <nlohmann/json.hpp>

#include <wx/wxprec.h>

#include <string>
#include <vector>
#include <map>

#ifdef _DEBUG
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#endif

#include "GLComponents.h"
#include "scene/Scene.h"
#include "scene/model/Model.h"
#include "scene/Camera.h"
#include "render/ShaderProgram.h"
#include "loaders/models/PlyLoader.h"
#include "scene/VisBox.h"
#include "scene/SceneApproximation.h"

using nlohmann::json;

Scene* InitialiseScene(std::string path, std::string filename);

std::vector<std::tuple<std::string, GLenum>> GetShaders(std::string base_path, nlohmann::json config, nlohmann::basic_json<> shader_config);

void CreateTexture(ShaderProgram* shader_program, std::string shader_name, std::string base_path, json image_specifier, float default_r = 1.0f, float default_g = 1.0f, float default_b = 1.0f);