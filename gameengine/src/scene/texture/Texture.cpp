#include "Texture.h"

#include <stdexcept>

#include "TextureFiltering.h"

constexpr std::size_t NUM_TEXTURE_CHANNELS = 3;

Texture::Texture(TextureReference reference) : Referenceable<TextureReference>(reference), m_filter_min(TextureFiltering::Nearest), m_filter_mag(TextureFiltering::Nearest)
{
}

void Texture::SetFullTexture(glm::vec3 colour, std::tuple<int, int> dimensions)
{
	const std::size_t pixels = std::size_t(std::get<0>(dimensions)) * std::size_t(std::get<1>(dimensions));

	std::vector<glm::vec3> data;
	data.reserve(pixels);
	for (std::size_t i = 0; i < pixels; i++)
	{
		data.push_back(colour);
	}

	this->SetFullTexture(data, dimensions);
}

void Texture::SetFullTexture(std::vector<glm::vec3> colour, std::tuple<int, int> dimensions)
{
	std::vector<unsigned char> texture;
	texture.reserve(NUM_TEXTURE_CHANNELS * colour.size());

	for (glm::vec3 pixel : colour)
	{
		for (glm::length_t i = 0; i < NUM_TEXTURE_CHANNELS; i++)
		{
			texture.push_back(Texture::NormalisedFloatToByte(pixel[i]));
		}
	}

	this->SetFullTexture(texture.data(), dimensions);
}

void Texture::SetVector(glm::vec3 colour)
{
	if (this->m_type != Type::Vector || this->m_vec_colour != colour)
	{
		this->IncrementRevisionIndex();

		this->m_type = Type::Vector;
		this->m_vec_colour = colour;

		this->m_data.clear();
		this->m_data.reserve(NUM_TEXTURE_CHANNELS);
		for (int i = 0; i < static_cast<int>(NUM_TEXTURE_CHANNELS); i++)
		{
			this->m_data.push_back(Texture::NormalisedFloatToByte(this->m_vec_colour[i]));
		}
	}
}

void Texture::SetFullTexture(const unsigned char* data, std::tuple<int, int> dimensions)
{
	if ((std::get<0>(dimensions) < 1) || (std::get<1>(dimensions) < 1))
	{
		throw std::invalid_argument("Texture dimensions must be greater than 1 in both axes");
	}

	this->IncrementRevisionIndex();

	this->m_type = Type::FullTexture;
	this->m_dimensions = dimensions;

	const std::size_t array_size = static_cast<std::size_t>(std::get<0>(dimensions)) * static_cast<std::size_t>(std::get<1>(dimensions)) * NUM_TEXTURE_CHANNELS;

	this->m_data.clear();
	this->m_data.reserve(array_size);
	this->m_data.assign(data, data + array_size);
}

std::array<unsigned char, 3> Texture::VecToBytes(glm::vec3 vec)
{
	return std::array<unsigned char, 3>({
		Texture::NormalisedFloatToByte(vec.x),
		Texture::NormalisedFloatToByte(vec.y),
		Texture::NormalisedFloatToByte(vec.z)
		});
}

glm::vec3 Texture::BytesToVec(std::array<unsigned char, 3> arr)
{
	return glm::vec3(
		Texture::ByteToNormalisedFloat(arr.at(0)),
		Texture::ByteToNormalisedFloat(arr.at(1)),
		Texture::ByteToNormalisedFloat(arr.at(2))
	);
}

unsigned char Texture::NormalisedFloatToByte(float value)
{
	return static_cast<unsigned char>(value * static_cast<float>((2 << 7) - 1));
}

float Texture::ByteToNormalisedFloat(unsigned char value)
{
	return static_cast<float>(value) / 0xFF;
}

std::size_t Texture::GetPixelIndex(int x, int y) const
{
	if (this->m_type == Type::FullTexture)
	{
		return NUM_TEXTURE_CHANNELS * (static_cast<std::size_t>(x) + (static_cast<std::size_t>(y) * std::get<0>(this->GetDimensions())));
	}
	else
	{
		throw std::runtime_error("Can't access individual pixels of a texture unless that texture is a full texture");
	}
}

glm::vec3 Texture::GetPixel(int x, int y) const
{
	std::size_t index = this->GetPixelIndex(x, y);
	return BytesToVec({
		index,
		index + 1,
		index + 2
		});
}

std::tuple<int, int> Texture::GetDimensions() const
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

const unsigned char* Texture::GetData() const
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

void Texture::SetPixel(int x, int y, glm::vec3 value)
{
	std::size_t index = this->GetPixelIndex(x, y);
	std::array<unsigned char, 3> value_bytes = Texture::VecToBytes(value);
	this->m_data.at(index) = value_bytes.at(0);
	this->m_data.at(index + 1) = value_bytes.at(1);
	this->m_data.at(index + 1) = value_bytes.at(2);
}

Texture::Type Texture::GetType() const
{
	return this->m_type;
}

glm::vec3 Texture::GetVector() const
{
	if (this->m_type == Type::Vector)
	{
		return this->m_vec_colour;
	}
	else
	{
		throw std::runtime_error("Texture must be a vector");
	}
}

void Texture::SetMagFilter(TextureFiltering filter)
{
	if (filter != this->m_filter_mag)
	{
		this->IncrementRevisionIndex();

		this->m_filter_mag = filter;
	}
}

TextureFiltering Texture::GetMagFilter() const
{
	return this->m_filter_mag;
}

void Texture::SetMinFilter(TextureFiltering filter)
{
	if (filter != this->m_filter_min)
	{
		this->IncrementRevisionIndex();

		this->m_filter_min = filter;
	}
}

TextureFiltering Texture::GetMinFilter() const
{
	return this->m_filter_min;
}

bool Texture::operator==(const Texture& second) const
{
	if (this == &second)
	{
		return true;
	}

	if (this->GetRevisionIndex() == second.GetRevisionIndex())
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

bool Texture::operator!=(const Texture& second) const
{
	return !(*this == second);
}