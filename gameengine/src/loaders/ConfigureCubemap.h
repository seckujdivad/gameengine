#pragma once

#include <memory>

#include <nlohmann/json.hpp>

class Scene;
class Cubemap;

void ConfigureCubemap(const nlohmann::json& data, const nlohmann::json& perf_data, Cubemap* cubemap, std::shared_ptr<Scene> scene);