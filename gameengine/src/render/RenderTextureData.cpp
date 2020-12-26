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

	if (info.colour)
	{
		glCopyImageSubData(source.colour, source.type, 0, 0, 0, 0, destination.colour, destination.type, 0, 0, 0, 0, std::get<0>(dimensions), std::get<1>(dimensions), source.type == GL_TEXTURE_CUBE_MAP ? 6 : 1);
	}

	if (info.depth)
	{
		glCopyImageSubData(source.depth, source.type, 0, 0, 0, 0, destination.depth, destination.type, 0, 0, 0, 0, std::get<0>(dimensions), std::get<1>(dimensions), source.type == GL_TEXTURE_CUBE_MAP ? 6 : 1);
	}

	for (int i = 0; i < static_cast<int>(source.data.size()); i++)
	{
		glCopyImageSubData(source.data.at(i), source.type, 0, 0, 0, 0, destination.data.at(i), destination.type, 0, 0, 0, 0, std::get<0>(dimensions), std::get<1>(dimensions), source.type == GL_TEXTURE_CUBE_MAP ? 6 : 1);
	}
}
