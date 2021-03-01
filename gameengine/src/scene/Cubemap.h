#pragma once

#include <tuple>
#include <vector>

#include "Referenceable.h"
#include "transformations/Positionable.h"

enum class CubemapType
{
	None,
	Reflection,
	Pointlight,
	Skybox
};

class Cubemap : public Positionable, public Referenceable<RenderTextureReference>
{
private:
	std::tuple<int, int> m_texture_dimensions = { 1, 1 };
	std::tuple<double, double> m_clips = { 0.1, 100.0 };

	int m_refresh_frames_required = 0;

	std::vector<ModelReference> m_models_static;
	std::vector<ModelReference> m_models_dynamic;

public:
	Cubemap(RenderTextureReference reference);

	void SetClips(std::tuple<double, double> clips);
	std::tuple<double, double> GetClips() const;

	void SetTextureDimensions(std::tuple<int, int> dimensions);
	std::tuple<int, int> GetTextureDimensions() const;

	void AddStaticModel(ModelReference reference);
	void RemoveStaticModel(ModelReference reference);
	bool ModelIsStatic(ModelReference reference) const;

	void AddDynamicModel(ModelReference reference);
	void RemoveDynamicModel(ModelReference reference);
	bool ModelIsDynamic(ModelReference reference) const;

	std::vector<ModelReference> GetStaticModels() const;
	std::vector<ModelReference> GetDynamicModels() const;

	void SetDynamicRedrawFrames(int num_frames);
	int GetDynamicRedrawFrames() const;
};