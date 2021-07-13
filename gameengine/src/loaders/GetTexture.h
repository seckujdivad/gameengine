#pragma once

#include <wx/image.h>

#include <nlohmann/json.hpp>

#include <glm/glm.hpp>

#include <filesystem>

#include "../scene/texture/Texture.h"

Texture GetTexture(const nlohmann::json& data, std::filesystem::path root_path, TextureReference reference, glm::vec3 default_value, TextureFiltering default_mag_filter, TextureFiltering default_min_filter);