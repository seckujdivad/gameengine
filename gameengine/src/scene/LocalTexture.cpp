#include "LocalTexture.h"

#include <stdexcept>

LocalTexture::LocalTexture(TextureReference reference) : Referenceable<TextureReference>(reference)
{
}

LocalTexture::LocalTexture(const LocalTexture& copy_from) : Referenceable<TextureReference>(copy_from)
{
	*this = copy_from;
}

LocalTexture& LocalTexture::operator=(const LocalTexture& copy_from)
{
	if (this == &copy_from)
	{
		return *this;
	}

	this->SetReference(copy_from.GetReference());
	this->m_type = copy_from.m_type;
	this->m_dimensions = copy_from.m_dimensions;
	this->m_vec_colour = copy_from.m_vec_colour;

	if (copy_from.m_full_data != nullptr)
	{
		this->m_full_data = new unsigned char[std::get<0>(this->m_dimensions) * std::get<1>(this->m_dimensions) * 3];
		std::memcpy(this->m_full_data, copy_from.m_full_data, sizeof(unsigned char) * std::get<0>(this->m_dimensions) * std::get<1>(this->m_dimensions) * 3);
	}

	if (copy_from.m_vec_data != nullptr)
	{
		this->m_vec_data = new unsigned char[3];
		std::memcpy(this->m_vec_data, copy_from.m_vec_data, sizeof(unsigned char) * 3);
	}

	return *this;
}

LocalTexture::LocalTexture(LocalTexture&& move_from) noexcept : Referenceable<TextureReference>(move_from)
{
	*this = std::move(move_from);
}

LocalTexture& LocalTexture::operator=(LocalTexture&& move_from) noexcept
{
	if (this == &move_from)
	{
		return *this;
	}

	this->SetReference(move_from.GetReference());
	this->m_type = move_from.m_type;
	this->m_dimensions = std::move(move_from.m_dimensions);
	this->m_vec_colour = std::move(move_from.m_vec_colour);

	this->m_full_data = move_from.m_full_data;
	move_from.m_full_data = nullptr;

	this->m_vec_data = move_from.m_vec_data;
	move_from.m_vec_data = nullptr;

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
	this->m_type = Type::Vector;

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
	this->m_type = Type::FullTexture;
	this->m_dimensions = dimensions;

	if (this->m_full_data != nullptr)
	{
		delete this->m_full_data;
		this->m_full_data = nullptr;
	}

	if (copy)
	{
		int num_chars = std::get<0>(dimensions) * std::get<1>(dimensions) * 3;
		this->m_full_data = new unsigned char[num_chars];
		std::memcpy(this->m_full_data, data, num_chars * sizeof(unsigned char));
	}
	else
	{
		this->m_full_data = data;
	}
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

unsigned char* LocalTexture::GetData() const
{
	if (this->m_type == Type::None)
	{
		throw std::runtime_error("Uninitialised texture accessed");
	}
	else if (this->m_type == Type::FullTexture)
	{
		return this->m_full_data;
	}
	else if (this->m_type == Type::Vector)
	{
		return this->m_vec_data;
	}
	else
	{
		throw std::runtime_error("Unrecognised texture type");
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
	if (this->m_type != second.m_type)
	{
		return false;
	}

	unsigned char* first_data = this->GetData();
	unsigned char* second_data = second.GetData();

	if (first_data == second_data)
	{
		return true;
	}
	else
	{
		if (this->m_filter_mag != second.m_filter_mag)
		{
			return false;
		}

		if (this->m_filter_min != second.m_filter_min)
		{
			return false;
		}

		std::tuple first_dimensions = this->GetDimensions();
		std::tuple second_dimensions = second.GetDimensions();

		if (first_dimensions != second_dimensions)
		{
			return false;
		}

		size_t num_pixels = static_cast<size_t>(std::get<0>(first_dimensions)) * static_cast<size_t>(std::get<1>(first_dimensions));

		if (num_pixels <= 32 * 32)
		{
			for (size_t i = 0; i < num_pixels; i++)
			{
				if (first_data[i] != second_data[i])
				{
					return false;
				}
			}
			return true;
		}
		else
		{
			return false; //too many pixels, skip the comparison
		}
	}
}

bool LocalTexture::operator!=(const LocalTexture& second) const
{
	return !(*this == second);
}
