#include "Generators.h"

#include <vector>
#include <stdexcept>
#include <string>

#include <glm/glm.hpp>

#include <wx/image.h>

void GenerateXORTexture(Texture& texture, std::tuple<int, int> dimensions, XORType type)
{
	const std::size_t pixels = std::size_t(std::get<0>(dimensions)) * std::size_t(std::get<1>(dimensions));

	std::vector<unsigned char> data;
	data.reserve(3 * pixels);
	for (std::size_t x = 0; x < std::size_t(std::get<0>(dimensions)); x++)
	{
		for (std::size_t y = 0; y < std::size_t(std::get<1>(dimensions)); y++)
		{
			std::size_t value = x ^ y;

			unsigned char r = 0;
			unsigned char g = 0;
			unsigned char b = 0;

			if (type == XORType::Greyscale)
			{
				r = static_cast<unsigned char>(value);
				g = static_cast<unsigned char>(value);
				b = static_cast<unsigned char>(value);
			}
			else if (type == XORType::HSV)
			{
				double hue = static_cast<double>(value % 256) / 256.0;
				double saturation = 1.0;
				double value = 1.0;

				wxImage::HSVValue hsv = wxImage::HSVValue(hue, saturation, value);

				wxImage::RGBValue rgb = wxImage::HSVtoRGB(hsv);

				r = rgb.red;
				g = rgb.green;
				b = rgb.blue;
			}
			else
			{
				throw std::invalid_argument("Unknown XORType \"" + std::to_string(static_cast<int>(type)) + "\"");
			}

			data.push_back(r);
			data.push_back(g);
			data.push_back(b);
		}
	}

	texture.SetFullTexture(data.data(), dimensions);
}
