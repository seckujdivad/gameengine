#pragma once

#include <glm/glm.hpp>

#include <tuple>

#include "Referenceable.h"

class LocalTexture : public Referenceable<TextureReference>
{
public:
	enum class Type
	{
		None,
		FullTexture,
		Vector
	};

	enum class Filter
	{
		Nearest,
		Linear
	};

private:
	Type m_type = Type::None;
	Filter m_filter_mag = Filter::Nearest;
	Filter m_filter_min = Filter::Nearest;

	//full texture
	unsigned char* m_full_data = nullptr;
	std::tuple<int, int> m_dimensions = { -1, -1 };

	//vector
	glm::vec3 m_vec_colour = glm::vec3(0.0f);
	unsigned char* m_vec_data = nullptr;

public:
	LocalTexture(TextureReference reference);
	LocalTexture(const LocalTexture& copy_from);
	LocalTexture& operator=(const LocalTexture& copy_from);
	LocalTexture(LocalTexture&& move_from) noexcept;
	LocalTexture& operator=(LocalTexture&& move_from) noexcept;
	~LocalTexture();

	void SetVector(glm::vec3 colour);

	void SetFullTexture(unsigned char* data, std::tuple<int, int> dimensions, bool copy = false);

	std::tuple<int, int> GetDimensions() const;
	unsigned char* GetData() const;

	void SetMagFilter(Filter filter);
	Filter GetMagFilter() const;
	void SetMinFilter(Filter filter);
	Filter GetMinFilter() const;

	bool operator==(const LocalTexture& second) const;
	bool operator!=(const LocalTexture& second) const;
};