#pragma once

#include <string>
#include <filesystem>
#include <map>
#include <vector>

#include <nlohmann/json.hpp>

#include "models/PlyLoader.h"
#include "../scene/LocalTexture.h"
#include "../scene/Referenceable.h"

class Scene;
class Cubemap;

Scene* SceneFromJSON(std::filesystem::path root_path, std::filesystem::path file_name);
Scene* SceneFromJSON(std::string root_path, std::string file_name);
Scene* SceneFromJSON(const char* root_path, const char* file_name);

template<unsigned int dimensions>
using dvec = glm::vec<dimensions, glm::f64, glm::packed_highp>;

template<unsigned int dimensions>
dvec<dimensions> GetVector(nlohmann::json data, dvec<dimensions> default_value);

LocalTexture GetTexture(nlohmann::json data, std::filesystem::path root_path, TextureReference reference, glm::vec3 default_value);

void ConfigureCubemap(nlohmann::json& data, Cubemap* cubemap, Scene* scene);