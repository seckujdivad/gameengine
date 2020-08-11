#pragma once

#include <tuple>
#include <vector>

#include "Referenceable.h"
#include "Positionable.h"

enum class CubemapType
{
	None,
	Reflection,
	Pointlight
};

class Cubemap : public Positionable, public Referenceable<RenderTextureReference>
{
private:
	std::tuple<int, int> m_texture_dimensions = { 1, 1 };
	std::tuple<double, double> m_clips = { 0.1, 100.0 };

	int m_refresh_frames_required = 0;
	int m_refresh_frames_elapsed = 0;

	std::vector<ModelReference> m_models_static;
	std::vector<ModelReference> m_models_dynamic;

public:
	Cubemap(RenderTextureReference reference);

	void SetClips(std::tuple<double, double> clips);
	std::tuple<double, double> GetClips();

	void SetTextureDimensions(std::tuple<int, int> dimensions);
	std::tuple<int, int> GetTextureDimensions();

	void AddStaticModel(ModelReference reference);
	void RemoveStaticModel(ModelReference reference);
	bool ModelIsStatic(ModelReference reference);

	void AddDynamicModel(ModelReference reference);
	void RemoveDynamicModel(ModelReference reference);
	bool ModelIsDynamic(ModelReference reference);

	std::vector<ModelReference> GetStaticModels();
	std::vector<ModelReference> GetDynamicModels();

	void SetFramesRequiredForDynamicRender(int num_frames);
	bool IsDynamicRenderRequired();
	void IncrementFrameCounter();
};