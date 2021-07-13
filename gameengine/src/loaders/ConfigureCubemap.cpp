#include "ConfigureCubemap.h"

#include <stdexcept>
#include <optional>

#include <glm/glm.hpp>

#include "GetVector.h"
#include "../scene/Cubemap.h"
#include "../scene/Scene.h"
#include "../scene/model/Model.h"

void ConfigureCubemap(const nlohmann::json& data, const nlohmann::json& perf_data, Cubemap* cubemap, std::shared_ptr<Scene> scene)
{
	glm::dvec2 clips = GetVector(data["clips"], glm::dvec2(std::get<0>(cubemap->GetClips()), std::get<1>(cubemap->GetClips())));
	cubemap->SetClips({ clips.x, clips.y });

	if (data.contains("texture") && data["texture"].is_number_integer())
	{
		glm::ivec2 tex_dimensions = GetVector(perf_data["scene"]["cubemap"]["texture"][data["texture"].get<int>()], glm::ivec2(1));
		cubemap->SetTextureDimensions({ tex_dimensions.x, tex_dimensions.y });
	}
	else
	{
		throw std::runtime_error("An index must be provided for the texture dimensions");
	}

	if (data.contains("static draw") && data["static draw"].is_array())
	{
		for (auto& el2 : data["static draw"].items())
		{
			if (el2.value().is_string())
			{
				std::optional<std::shared_ptr<Model>> model = scene->GetModel(el2.value().get<std::string>());
				if (model.has_value())
				{
					cubemap->AddStaticModel(model.value()->GetReference());
				}
				else
				{
					throw std::runtime_error("Cubemap static draw target '" + el2.value().get<std::string>() + "' does not exist");
				}
			}
			else
			{
				throw std::runtime_error("Cubemap static draw targets must be provided as string identifiers");
			}
		}
	}

	if (data.contains("dynamic draw") && data["dynamic draw"].is_array())
	{
		for (auto& el2 : data["dynamic draw"].items())
		{
			if (el2.value().is_string())
			{
				std::optional<std::shared_ptr<Model>> model = scene->GetModel(el2.value().get<std::string>());
				if (model.has_value())
				{
					cubemap->AddDynamicModel(model.value()->GetReference());
				}
				else
				{
					throw std::runtime_error("Cubemap dynamic draw target '" + el2.value().get<std::string>() + "' does not exist");
				}
			}
			else
			{
				throw std::runtime_error("Cubemap dynamic draw targets must be provided as string identifiers");
			}
		}
	}

	if (data.contains("dynamic draw refresh frames") && data["dynamic draw refresh frames"].is_number())
	{
		int dynamic_redraw_frames = data["dynamic draw refresh frames"].get<int>();
		if (dynamic_redraw_frames > 0)
		{
			cubemap->SetDynamicRedrawFrames(dynamic_redraw_frames);
		}
		else
		{
			throw std::runtime_error("Cubemap dynamic redraw frames must be a positive integer");
		}
	}
}