#pragma once

#include <vector>
#include <variant>
#include <memory>

#include <glm/glm.hpp>

class RenderTextureGroup;
class GLTexture;
enum class RenderTargetMode;
enum class RenderTargetModeType;

struct RenderTargetConfig
{
	struct Default
	{
	};

	struct Normal_DepthOnly
	{
	};

	struct Normal_Draw
	{
		std::shared_ptr<RenderTextureGroup> depth_frame;
		std::shared_ptr<RenderTextureGroup> ssr_quality_frame;
		bool draw_shadows = true;
		bool draw_reflections = true;
	};

	struct Normal_SSRQuality
	{
		std::shared_ptr<RenderTextureGroup> draw_frame;
	};

	struct Normal_PostProcess
	{
		std::shared_ptr<RenderTextureGroup> draw_frame;
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
		enum class Mode
		{
			Uninitialised = -1,

			/*
			* Blends every layer together from the start to the end of "layers"
			* Implicitly uses the background colour as the first layer, then
			* blends with the first, second, third etc layer in "layers"
			*/
			AlphaBlend
		};
		
		struct Uninitialised
		{
		};

		struct AlphaBlend
		{
		};

		std::variant<Uninitialised, AlphaBlend> data;

		Mode GetMode() const;

		template<class T>
		inline T& Data()
		{
			return std::get<T>(this->data);
		}

		template<class T>
		inline const T& Data() const
		{
			return std::get<T>(this->data);
		}

		struct Layer
		{
			GLTexture* texture;
			glm::vec4 colour_translate = glm::vec4(0.0f);
			glm::vec4 colour_scale = glm::vec4(1.0f);
		};
		std::vector<Layer> layers;
	};

	struct Textured
	{
	};

	using ModeData = std::variant<Default, Normal_DepthOnly, Normal_Draw, Normal_SSRQuality, Normal_PostProcess, Wireframe, Shadow, PostProcess, Textured>;

	RenderTargetConfig();
	RenderTargetConfig(RenderTargetMode mode);
	RenderTargetConfig(ModeData mode_data);

	ModeData mode_data;
	bool clear_fbo = true;

	void SetMode(RenderTargetMode mode);
	RenderTargetMode GetMode() const;

	template<class T>
	inline T& Data()
	{
		return std::get<T>(this->mode_data);
	}

	template<class T>
	inline const T& Data() const
	{
		return std::get<T>(this->mode_data);
	}
};