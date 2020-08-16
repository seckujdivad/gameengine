#include "LocalTexture.h"

LocalTexture::LocalTexture(TextureReference reference) : Referenceable<TextureReference>(reference)
{
}

LocalTexture::LocalTexture(const LocalTexture& copy_from) : Referenceable<TextureReference>(copy_from)
{
	*this = copy_from;
}

LocalTexture& LocalTexture::operator=(const LocalTexture& copy_from)
{
	this->m_type = copy_from.m_type;

	this->m_dimensions = copy_from.m_dimensions;
	if (copy_from.m_full_data != nullptr)
	{
		this->m_full_data = new unsigned char[std::get<0>(this->m_dimensions) * std::get<1>(this->m_dimensions)];
		std::memcpy(this->m_full_data, copy_from.m_full_data, sizeof(unsigned char) * std::get<0>(this->m_dimensions) * std::get<1>(this->m_dimensions));
	}

	this->m_vec_colour = copy_from.m_vec_colour;
	if (copy_from.m_vec_data != nullptr)
	{
		this->m_vec_data = new unsigned char[3];
		std::memcpy(this->m_vec_data, copy_from.m_vec_data, sizeof(unsigned char) * 3);
	}

	return *this;
}

LocalTexture::~LocalTexture()
{
	if (this->m_vec_data != nullptr)
	{
		delete this->m_vec_data;
	}

	if (this->m_full_data != nullptr)
	{
		delete this->m_full_data;
	}
}

void LocalTexture::SetVector(glm::vec3 colour)
{
	this->m_type = LocalTextureType::Vector;

	if (this->m_vec_data != nullptr)
	{
		delete this->m_vec_data;
		this->m_vec_data = nullptr;
	}

	this->m_vec_colour = colour;

	this->m_vec_data = new unsigned char[3];
	this->m_vec_data[0] = (unsigned char)(this->m_vec_colour.r * ((2 << 7) - 1));
	this->m_vec_data[1] = (unsigned char)(this->m_vec_colour.g * ((2 << 7) - 1));
	this->m_vec_data[2] = (unsigned char)(this->m_vec_colour.b * ((2 << 7) - 1));
}

void LocalTexture::SetFullTexture(unsigned char* data, std::tuple<int, int> dimensions, bool copy)
{
	this->m_type = LocalTextureType::FullTexture;
	this->m_dimensions = dimensions;

	if (this->m_full_data != nullptr)
	{
		delete this->m_full_data;
		this->m_full_data = nullptr;
	}

	if (copy)
	{
		int num_chars = std::get<0>(dimensions) * std::get<1>(dimensions);
		this->m_full_data = new unsigned char[num_chars];
		std::memcpy(this->m_full_data, data, num_chars * sizeof(unsigned char));
	}
	else
	{
		this->m_full_data = data;
	}
}

std::tuple<int, int> LocalTexture::GetDimensions()
{
	if (this->m_type == LocalTextureType::None)
	{
		throw std::runtime_error("Uninitialised texture accessed");
	}
	else if (this->m_type == LocalTextureType::FullTexture)
	{
		return this->m_dimensions;
	}
	else if (this->m_type == LocalTextureType::Vector)
	{
		return { 1, 1 };
	}
	else
	{
		throw std::runtime_error("Unrecognised texture type");
	}

	return { -1, -1 };
}

unsigned char* LocalTexture::GetData()
{
	if (this->m_type == LocalTextureType::None)
	{
		throw std::runtime_error("Uninitialised texture accessed");
	}
	else if (this->m_type == LocalTextureType::FullTexture)
	{
		return this->m_full_data;
	}
	else if (this->m_type == LocalTextureType::Vector)
	{
		return this->m_vec_data;
	}
	else
	{
		throw std::runtime_error("Unrecognised texture type");
	}
}

void LocalTexture::SetMagFilter(LocalTextureFilter filter)
{
	this->m_filter_mag = filter;
}

LocalTextureFilter LocalTexture::GetMagFilter()
{
	return this->m_filter_mag;
}

void LocalTexture::SetMinFilter(LocalTextureFilter filter)
{
	this->m_filter_min = filter;
}

LocalTextureFilter LocalTexture::GetMinFilter()
{
	return this->m_filter_min;
}
