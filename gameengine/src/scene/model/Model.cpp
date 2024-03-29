#include "Model.h"

#include <stdexcept>

#include "../Scene.h"
#include "geometry/Geometry.h"

Model::Model(ModelReference reference, std::vector<std::shared_ptr<Geometry>> geometry, Scene* scene)
	:
	Positionable(),
	Rotatable(),
	Scalable(),
	Referenceable<ModelReference>(reference),
	SceneChild(scene),
	m_geometry(geometry),
	m_motion_position(glm::dvec3(0.0), glm::dvec3(0.0)),
	m_motion_rotation(glm::dvec3(0.0), glm::dvec3(0.0)),
	m_motion_scale(glm::dvec3(0.0), glm::dvec3(0.0))
{
	this->m_texture_colour = Texture(scene->GetNewTextureReference());
	this->m_texture_reflection = Texture(scene->GetNewTextureReference());
	this->m_texture_specular = Texture(scene->GetNewTextureReference());
	this->m_texture_normal = Texture(scene->GetNewTextureReference());
	this->m_texture_skybox_mask = Texture(scene->GetNewTextureReference());
	this->m_texture_displacement = Texture(scene->GetNewTextureReference());

	this->DefaultInitialiseTextures();
}

Material& Model::GetMaterial()
{
	return this->m_material;
}

const Material& Model::GetMaterial() const
{
	return this->m_material;
}

void Model::AddGeometry(std::shared_ptr<Geometry> geometry)
{
	this->m_geometry.push_back(geometry);
}

void Model::AddGeometry(std::vector<std::shared_ptr<Geometry>> geometry)
{
	for (const std::shared_ptr<Geometry>& single_geometry : geometry)
	{
		this->AddGeometry(single_geometry);
	}
}

void Model::SetGeometry(std::vector<std::shared_ptr<Geometry>> geometry)
{
	this->m_geometry = geometry;
}

void Model::ClearGeometry()
{
	this->m_geometry.clear();
}

const std::vector<std::shared_ptr<Geometry>>& Model::GetGeometry() const
{
	return this->m_geometry;
}

Texture& Model::GetColourTexture()
{
	return this->m_texture_colour;
}

const Texture& Model::GetColourTexture() const
{
	return this->m_texture_colour;
}

Texture& Model::GetReflectionTexture()
{
	return this->m_texture_reflection;
}

const Texture& Model::GetReflectionTexture() const
{
	return this->m_texture_reflection;
}

Texture& Model::GetSpecularTexture()
{
	return this->m_texture_specular;
}

const Texture& Model::GetSpecularTexture() const
{
	return this->m_texture_specular;
}

Texture& Model::GetNormalTexture()
{
	return this->m_texture_normal;
}

const Texture& Model::GetNormalTexture() const
{
	return this->m_texture_normal;
}

Texture& Model::GetSkyboxMaskTexture()
{
	return this->m_texture_skybox_mask;
}

const Texture& Model::GetSkyboxMaskTexture() const
{
	return this->m_texture_skybox_mask;
}

Texture& Model::GetDisplacementTexture()
{
	return this->m_texture_displacement;
}

const Texture& Model::GetDisplacementTexture() const
{
	return this->m_texture_displacement;
}

void Model::DefaultInitialiseTextures()
{
	this->m_texture_colour.SetVector(glm::vec3(1.0f));
	this->m_texture_reflection.SetVector(glm::vec3(0.0f));
	this->m_texture_specular.SetVector(glm::vec3(0.0f));
	this->m_texture_normal.SetVector(glm::vec3(0.5f, 0.5f, 1.0f));
	this->m_texture_skybox_mask.SetVector(glm::vec3(0.0f));
	this->m_texture_displacement.SetVector(glm::vec3(0.0f));
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

void Model::SetSkybox(std::shared_ptr<Skybox> skybox)
{
	this->m_skybox = skybox;
}

std::shared_ptr<Skybox> Model::GetSkybox() const
{
	return this->m_skybox;
}

MotionDescriptor<glm::dvec3>& Model::GetPositionMotionDescriptor()
{
	return this->m_motion_position;
}

const MotionDescriptor<glm::dvec3>& Model::GetPositionMotionDescriptor() const
{
	return this->m_motion_position;
}

MotionDescriptor<glm::dvec3>& Model::GetRotationMotionDescriptor()
{
	return this->m_motion_rotation;
}

const MotionDescriptor<glm::dvec3>& Model::GetRotationMotionDescriptor() const
{
	return this->m_motion_rotation;
}

MotionDescriptor<glm::dvec3>& Model::GetScaleMotionDescriptor()
{
	return this->m_motion_scale;
}

const MotionDescriptor<glm::dvec3>& Model::GetScaleMotionDescriptor() const
{
	return this->m_motion_scale;
}
