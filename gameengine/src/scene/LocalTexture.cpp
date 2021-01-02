#include "LocalTexture.h"

#include <stdexcept>

const int NUM_TEXTURE_CHANNELS = 3;

LocalTexture::LocalTexture(TextureReference reference) : Referenceable<TextureReference>(reference)
{
}

void LocalTexture::SetVector(glm::vec3 colour)
{
	this->m_type = Type::Vector;

	this->m_vec_colour = colour;

	this->m_data.clear();
	this->m_data.reserve(NUM_TEXTURE_CHANNELS);
	for (int i = 0; i < NUM_TEXTURE_CHANNELS; i++)
	{
		this->m_data.push_back(static_cast<unsigned char>(this->m_vec_colour[i] * ((2 << 7) - 1)));
	}
}

void LocalTexture::SetFullTexture(const unsigned char* data, std::tuple<int, int> dimensions)
{
	if ((std::get<0>(dimensions) < 1) || (std::get<1>(dimensions) < 1))
	{
		throw std::invalid_argument("Texture dimensions must be greater than 1 in both axes");
	}

	this->m_type = Type::FullTexture;
	this->m_dimensions = dimensions;

	std::size_t array_size = static_cast<std::size_t>(std::get<0>(dimensions)) * static_cast<std::size_t>(std::get<1>(dimensions)) * static_cast<std::size_t>(NUM_TEXTURE_CHANNELS);

	this->m_data.clear();
	this->m_data.reserve(array_size);
	this->m_data.assign(data, data + array_size);
}

std::tuple<int, int> LocalTexture::GetDimensions() const
{
	if (this->m_type == Type::None)
	{
		throw std::runtime_error("Uninitialised texture accessed");
	}
	else if (this->m_type == Type::FullTexture)
	{
		return this->m_dimensions;
	}
	else if (this->m_type == Type::Vector)
	{
		return std::tuple(1, 1);
	}
	else
	{
		throw std::runtime_error("Unrecognised texture type");
	}

	return std::tuple(-1, -1);
}

const unsigned char* LocalTexture::GetData() const
{
	if (this->m_type == Type::None)
	{
		throw std::runtime_error("Uninitialised texture accessed");
	}
	else
	{
		return this->m_data.data();
	}
}

void LocalTexture::SetMagFilter(Filter filter)
{
	this->m_filter_mag = filter;
}

LocalTexture::Filter LocalTexture::GetMagFilter() const
{
	return this->m_filter_mag;
}

void LocalTexture::SetMinFilter(Filter filter)
{
	this->m_filter_min = filter;
}

LocalTexture::Filter LocalTexture::GetMinFilter() const
{
	return this->m_filter_min;
}

bool LocalTexture::operator==(const LocalTexture& second) const
{
	if (this == &second)
	{
		return true;
	}

	if (this->m_type != second.m_type)
	{
		return false;
	}

	if ((this->m_type == Type::None) || (second.m_type == Type::None))
	{
		return false;
	}

	if (this->m_filter_mag != second.m_filter_mag)
	{
		return false;
	}

	if (this->m_filter_min != second.m_filter_min)
	{
		return false;
	}

	if (this->GetDimensions() != second.GetDimensions())
	{
		return false;
	}

	return this->m_data == second.m_data;
}

bool LocalTexture::operator!=(const LocalTexture& second) const
{
	return !(*this == second);
}
