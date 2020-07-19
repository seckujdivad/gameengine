#pragma once

#include <wx/wxprec.h>
#include "../../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <vector>
#include <tuple>

#include "../../render/ShaderProgram.h"
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

	bool ssr_enabled;
	MaterialSSRConfig ssr;

	std::vector<std::tuple<Reflection*, ReflectionMode>> reflections;
};
