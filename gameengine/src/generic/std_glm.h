#pragma once

#include <vector>

#include <glm/glm.hpp>

template<glm::length_t L, typename T>
glm::vec<L, T> GetGLMVector(std::vector<T> vector);

template<glm::length_t L, typename T>
std::vector<T> GetSTDVector(glm::vec<L, T> vector);