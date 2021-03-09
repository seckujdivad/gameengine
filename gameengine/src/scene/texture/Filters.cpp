#include "Filters.h"

#include <stdexcept>

constexpr glm::vec3 GREYSCALE_WEIGHTS = glm::vec3(0.3f, 0.59f, 0.11f);

glm::vec3 MakeGreyscale(glm::vec3 target)
{
	return glm::vec3(target * GREYSCALE_WEIGHTS);
}

void MakeGreyscale(Texture& target)
{
	if (target.GetType() == Texture::Type::FullTexture)
	{
		const auto& [size_x, size_y] = target.GetDimensions();

		for (int y = 0; y < size_y; y++)
		{
			for (int x = 0; x < size_x; x++)
			{
				target.SetPixel(x, y, MakeGreyscale(target.GetPixel(x, y)));
			}
		}
	}
	else if (target.GetType() == Texture::Type::Vector)
	{
		target.SetVector(MakeGreyscale(target.GetVector()));
	}
	else
	{
		throw std::invalid_argument("Texture must be initialised");
	}
}
