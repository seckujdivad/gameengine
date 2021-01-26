#include "RenderTextureData.h"

#include <stdexcept>

bool operator==(const RenderTextureInfo& first, const RenderTextureInfo& second)
{
	if (first.colour != second.colour)
	{
		return false;
	}

	if (first.colour_filtering != second.colour_filtering)
	{
		return false;
	}

	if (first.depth != second.depth)
	{
		return false;
	}

	if (first.depth_filtering != second.depth_filtering)
	{
		return false;
	}

	if (first.num_data != second.num_data)
	{
		return false;
	}

	if (first.data_filtering != second.data_filtering)
	{
		return false;
	}

	return true;
}

bool operator!=(const RenderTextureInfo& first, const RenderTextureInfo& second)
{
	return !(first == second);
}

void CopyTextureGroup(RenderTextureGroup source, RenderTextureGroup destination, RenderTextureInfo info, std::tuple<int, int> dimensions)
{
	if (source.data.size() != destination.data.size())
	{
		throw std::invalid_argument("Source data size and destination data size must match - source data has size " + std::to_string(source.data.size()) + " and destination data has size " + std::to_string(destination.data.size()));
	}

	GLint copy_layers = source.type == GL_TEXTURE_CUBE_MAP ? 6 : 1;

	if (info.colour)
	{
#ifdef _DEBUG
		if (!glIsTexture(source.colour))
		{
			throw std::invalid_argument("Source colour texture (" + std::to_string(source.colour) + ") is invalid");
		}

		if (!glIsTexture(destination.colour))
		{
			throw std::invalid_argument("Destination colour texture (" + std::to_string(destination.colour) + ") is invalid");
		}
#endif
		glCopyImageSubData(source.colour, source.type, 0, 0, 0, 0, destination.colour, destination.type, 0, 0, 0, 0, std::get<0>(dimensions), std::get<1>(dimensions), copy_layers);
	}

	if (info.depth)
	{
#ifdef _DEBUG
		if (!glIsTexture(source.depth))
		{
			throw std::invalid_argument("Source depth texture (" + std::to_string(source.depth) + ") is invalid");
		}

		if (!glIsTexture(destination.depth))
		{
			throw std::invalid_argument("Destination depth texture (" + std::to_string(destination.depth) + ") is invalid");
		}
#endif
		glCopyImageSubData(source.depth, source.type, 0, 0, 0, 0, destination.depth, destination.type, 0, 0, 0, 0, std::get<0>(dimensions), std::get<1>(dimensions), copy_layers);
	}

	for (int i = 0; i < static_cast<int>(source.data.size()); i++)
	{
#ifdef _DEBUG
		if (!glIsTexture(source.data.at(i)))
		{
			throw std::invalid_argument("Source data " + std::to_string(i) + " texture (" + std::to_string(source.data.at(i)) + ") is invalid");
		}

		if (!glIsTexture(destination.data.at(i)))
		{
			throw std::invalid_argument("Destination data " + std::to_string(i) +  " texture (" + std::to_string(destination.data.at(i)) + ") is invalid");
		}
#endif
		glCopyImageSubData(source.data.at(i), source.type, 0, 0, 0, 0, destination.data.at(i), destination.type, 0, 0, 0, 0, std::get<0>(dimensions), std::get<1>(dimensions), copy_layers);
	}
}

bool CheckTextureGroup(RenderTextureGroup texture_group, RenderTextureInfo texture_info)
{
	bool correct = (texture_info.colour) == (texture_group.colour != NULL);
	correct = correct && (texture_info.depth) == (texture_group.depth != NULL);
	correct = correct && (texture_info.num_data == static_cast<int>(texture_group.data.size()));

	return correct;
}
