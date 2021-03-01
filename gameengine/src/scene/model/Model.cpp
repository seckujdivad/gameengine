#include "Model.h"

#include <array>
#include <cmath>
#include <set>
#include <map>
#include <stdexcept>

#include "../Scene.h"
#include "geometry/Geometry.h"

Model::Model(ModelReference reference, std::vector<std::shared_ptr<Geometry>> geometry, Scene* scene)
	:
	Positionable(),
	Rotatable(),
	Nameable(),
	Scalable(),
	Referenceable<ModelReference>(reference),
	SceneChild(scene),
	m_geometry(geometry)
{
	if (scene == nullptr)
	{
		this->m_texture_colour = Texture(-1);
		this->m_texture_reflection = Texture(-1);
		this->m_texture_specular = Texture(-1);
		this->m_texture_normal = Texture(-1);
		this->m_texture_skybox_mask = Texture(-1);
		this->m_texture_displacement = Texture(-1);
	}
	else
	{
		this->m_texture_colour = Texture(scene->GetNewTextureReference());
		this->m_texture_reflection = Texture(scene->GetNewTextureReference());
		this->m_texture_specular = Texture(scene->GetNewTextureReference());
		this->m_texture_normal = Texture(scene->GetNewTextureReference());
		this->m_texture_skybox_mask = Texture(scene->GetNewTextureReference());
		this->m_texture_displacement = Texture(scene->GetNewTextureReference());
	}
}

Material& Model::GetMaterial()
{
	return this->m_material;
}

std::vector<std::shared_ptr<Geometry>> Model::GetGeometry()
{
	return this->m_geometry;
}

Texture& Model::GetColourTexture()
{
	return this->m_texture_colour;
}

Texture& Model::GetReflectionTexture()
{
	return this->m_texture_reflection;
}

Texture& Model::GetSpecularTexture()
{
	return this->m_texture_specular;
}

Texture& Model::GetNormalTexture()
{
	return this->m_texture_normal;
}

Texture& Model::GetSkyboxMaskTexture()
{
	return this->m_texture_skybox_mask;
}

Texture& Model::GetDisplacementTexture()
{
	return this->m_texture_displacement;
}

void Model::SetWireframeColours(std::vector<glm::vec3> colours)
{
	if (colours.size() == 0)
	{
		throw std::invalid_argument("At least one colour must be provided");
	}
	else if (static_cast<int>(colours.size()) <= this->m_wireframe_colours_index)
	{
		throw std::invalid_argument("The provided colour list contains " + std::to_string(colours.size()) + " colours, but colour number " + std::to_string(this->m_wireframe_colours_index + 1) + "is selected");
	}
	else
	{
		this->m_wireframe_colours = colours;
	}
}

void Model::SetCurrentWireframeIndex(int index)
{
	if (index < 0)
	{
		throw std::invalid_argument("Index must be positive or zero");
	}
	else if (static_cast<int>(this->m_wireframe_colours.size()) <= index)
	{
		throw std::invalid_argument("Index must be a valid index of the provided wireframe colours list (which has a length of " + std::to_string(this->m_wireframe_colours.size()) + ")");
	}
	else
	{
		this->m_wireframe_colours_index = index;
	}
}

void Model::SetCurrentWireframeColour(glm::vec3 colour)
{
	this->m_wireframe_colours.at(this->m_wireframe_colours_index) = colour;
}

glm::vec3 Model::GetCurrentWireframeColour() const
{
	return this->m_wireframe_colours.at(this->m_wireframe_colours_index);
}

void Model::SetSkybox(Skybox* skybox)
{
	this->m_skybox = skybox;
}

Skybox* Model::GetSkybox() const
{
	return this->m_skybox;
}
