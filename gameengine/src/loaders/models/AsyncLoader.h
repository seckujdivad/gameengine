#pragma once

#include <vector>
#include <string>

#include <nlohmann/json.hpp>

class Model;

void LoadModelGeometry(std::string root, std::vector<std::shared_ptr<Model>> models, nlohmann::json layout, nlohmann::json geometry_info);