#include "Cubemap.h"

Cubemap::Cubemap(RenderTextureReference reference) : Positionable(), Referenceable<RenderTextureReference>(reference)
{
}

void Cubemap::SetClips(std::tuple<double, double> clips)
{
	this->m_clips = clips;
}

std::tuple<double, double> Cubemap::GetClips() const
{
	return this->m_clips;
}

void Cubemap::SetTextureDimensions(std::tuple<int, int> dimensions)
{
	this->m_texture_dimensions = dimensions;
}

std::tuple<int, int> Cubemap::GetTextureDimensions() const
{
	return this->m_texture_dimensions;
}

void Cubemap::AddStaticModel(ModelReference reference)
{
	if (!this->ModelIsStatic(reference))
	{
		this->m_models_static.push_back(reference);
	}
}

void Cubemap::RemoveStaticModel(ModelReference reference)
{
	std::vector<ModelReference>::iterator it = std::find(this->m_models_static.begin(), this->m_models_static.end(), reference);
	while (it != this->m_models_static.end())
	{
		this->m_models_static.erase(it);
	}
}

bool Cubemap::ModelIsStatic(ModelReference reference) const
{
	return std::find(this->m_models_static.begin(), this->m_models_static.end(), reference) != this->m_models_static.end();
}

void Cubemap::AddDynamicModel(ModelReference reference)
{
	if (!this->ModelIsDynamic(reference))
	{
		this->m_models_dynamic.push_back(reference);
	}
}

void Cubemap::RemoveDynamicModel(ModelReference reference)
{
	std::vector<ModelReference>::iterator it = std::find(this->m_models_dynamic.begin(), this->m_models_dynamic.end(), reference);
	while (it != this->m_models_dynamic.end())
	{
		this->m_models_dynamic.erase(it);
	}
}

bool Cubemap::ModelIsDynamic(ModelReference reference) const
{
	return std::find(this->m_models_dynamic.begin(), this->m_models_dynamic.end(), reference) != this->m_models_dynamic.end();
}

std::vector<ModelReference> Cubemap::GetStaticModels() const
{
	return this->m_models_static;
}

std::vector<ModelReference> Cubemap::GetDynamicModels() const
{
	return this->m_models_dynamic;
}

void Cubemap::SetDynamicRedrawFrames(int num_frames)
{
	this->m_refresh_frames_required = num_frames;
}

int Cubemap::GetDynamicRedrawFrames() const
{
	return this->m_refresh_frames_required;
}
