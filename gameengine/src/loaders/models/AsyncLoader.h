#pragma once

#include <vector>

#include <nlohmann/json.hpp>

class Model;

void LoadModelGeometry(std::vector<std::shared_ptr<Model>> models, nlohmann::json layout, nlohmann::json geometry_info);