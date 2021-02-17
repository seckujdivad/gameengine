#pragma once

#include <vector>
#include <variant>
#include <memory>

#include <glm/glm.hpp>

class RenderTextureGroup;
class Texture;
enum class RenderTargetMode;
enum class RenderTargetModeType;

struct RenderTargetConfig
{
	struct Normal_DepthOnly
	{
	};

	struct Normal_Draw
	{
		std::shared_ptr<RenderTextureGroup> previous_frame;
		bool draw_shadows = true;
		bool draw_reflections = true;
	};

	struct Wireframe
	{
		bool draw_back_faces = true;
	};

	struct Shadow
	{

	};

	struct PostProcess
	{
		struct CompositeLayer
		{
			Texture* texture;
			glm::vec4 colour_translate = glm::vec4(0.0f);
			glm::vec4 colour_scale = glm::vec4(1.0f);
		};
		std::vector<CompositeLayer> layers;
	};

	struct Textured
	{

	};

	using ModeData = std::variant<Normal_DepthOnly, Normal_Draw, Wireframe, Shadow, PostProcess, Textured>;

	RenderTargetConfig();
	RenderTargetConfig(RenderTargetMode mode, ModeData mode_data);

	RenderTargetMode mode;
	ModeData mode_data;

	bool clear_fbo = true;

	void SetMode(RenderTargetMode mode);
};

void SetMode(RenderTargetConfig& config, RenderTargetMode mode);