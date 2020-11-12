#pragma once

#include <glm/glm.hpp>

#include <vector>
#include <tuple>

#include "Reflection.h"

struct MaterialSSRConfig
{
	float resolution = 1.0f;
	float cast_distance_limit = 1.0f;
	float depth_acceptance = 0.1f;
	float max_cam_distance = 10.0f;
	bool appear_in_ssr = false;
	int refinements = 1;
};

struct Material
{
	glm::vec3 diffuse = glm::vec3(0.0f);
	glm::vec3 specular = glm::vec3(0.0f);
	float specular_highlight = 2.0f;

	bool ssr_enabled = true;
	MaterialSSRConfig ssr;

	bool reflections_enabled = true;
	std::vector<std::tuple<Reflection*, ReflectionMode>> reflections;

	float displacement_multiplier = 1.0f;
};
