#pragma once

#include <glm/glm.hpp>

#include <nlohmann/json.hpp>

template<unsigned int dimensions, typename T = double>
using vec = glm::vec<dimensions, T, glm::packed_highp>;

template<unsigned int dimensions, typename T = double>
vec<dimensions, T> GetVector(const nlohmann::json& data, vec<dimensions, T> default_value);