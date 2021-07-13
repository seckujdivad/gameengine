#include "GetTexture.h"

#include <stdexcept>

#include "GetVector.h"
#include "../scene/texture/Texture.h"
#include "../scene/texture/TextureFiltering.h"
#include "../scene/texture/Generators.h"

Texture GetTexture(const nlohmann::json& data, std::filesystem::path root_path, TextureReference reference, glm::vec3 default_value, TextureFiltering default_mag_filter, TextureFiltering default_min_filter)
{
	Texture texture(reference);
	texture.SetMagFilter(default_mag_filter);
	texture.SetMinFilter(default_min_filter);

	if (data.is_string())
	{
		wxImage image;
		std::filesystem::path img_path = root_path / data.get<std::string>();
		image.LoadFile(img_path.string());

		if (!image.IsOk())
		{
			throw std::runtime_error("Error while loading image at '" + img_path.string() + "'");
		}

		texture.SetFullTexture(image.GetData(), { image.GetWidth(), image.GetHeight() });
	}
	else if (data.is_object())
	{
		if (data.contains("texture"))
		{
			if (data["texture"].is_string())
			{
				texture = GetTexture(data["texture"], root_path, reference, default_value, default_mag_filter, default_min_filter);
			}
			else if (data["texture"].is_object() && data["texture"].contains("preset") && data["texture"]["preset"].is_string())
			{
				const std::string preset = data["texture"]["preset"].get<std::string>();
				if (preset == "xor")
				{
					XORType xor_type = XORType::Greyscale;
					if (data["texture"].contains("mode") && data["texture"]["mode"].is_string())
					{
						const std::string xor_type_string = data["texture"]["mode"].get<std::string>();
						if (xor_type_string == "greyscale")
						{
							xor_type = XORType::Greyscale;
						}
						else if (xor_type_string == "hsv")
						{
							xor_type = XORType::HSV;
						}
						else
						{
							throw std::runtime_error("Invalid XOR type \"" + xor_type_string + "\"");
						}
					}

					glm::ivec2 dimensions = GetVector(data["texture"]["dimensions"], glm::ivec2(1, 1));

					GenerateXORTexture(texture, std::tuple<int, int>(dimensions.x, dimensions.y), xor_type);
				}
				else
				{
					throw std::runtime_error("Invalid preset \"" + preset + "\"");
				}
			}
			else
			{
				throw std::runtime_error("Texture must be specified as either an object containing a string member \"preset\" or a string");
			}
		}
		else
		{
			throw std::runtime_error("No texture specified");
		}

		if (data.contains("magnify filter") && data["magnify filter"].is_string())
		{
			std::string filter = data["magnify filter"].get<std::string>();
			if (filter == "nearest")
			{
				texture.SetMagFilter(TextureFiltering::Nearest);
			}
			else if (filter == "linear")
			{
				texture.SetMagFilter(TextureFiltering::Linear);
			}
			else
			{
				throw std::runtime_error("Magnify filter must be either 'linear' or 'nearest', not '" + filter + "'");
			}
		}

		if (data.contains("shrink filter") && data["shrink filter"].is_string())
		{
			std::string filter = data["shrink filter"].get<std::string>();
			if (filter == "nearest")
			{
				texture.SetMinFilter(TextureFiltering::Nearest);
			}
			else if (filter == "linear")
			{
				texture.SetMinFilter(TextureFiltering::Linear);
			}
			else
			{
				throw std::runtime_error("Shrink filter must be either 'linear' or 'nearest', not '" + filter + "'");
			}
		}
	}
	else
	{
		texture.SetVector(GetVector(data, glm::dvec3(default_value)));
	}

	return texture;
}