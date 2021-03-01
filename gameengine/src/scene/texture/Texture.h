#pragma once

#include <glm/glm.hpp>

#include <tuple>
#include <vector>

#include "../Referenceable.h"
#include "../RevisableResource.h"

class Texture : public Referenceable<TextureReference>, public RevisableResource
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

	std::vector<unsigned char> m_data;

	//full texture
	std::tuple<int, int> m_dimensions = { -1, -1 };

	//vector
	glm::vec3 m_vec_colour = glm::vec3(0.0f);

public:
	Texture(TextureReference reference);

	void SetVector(glm::vec3 colour);

	void SetFullTexture(const unsigned char* data, std::tuple<int, int> dimensions);

	std::tuple<int, int> GetDimensions() const;
	const unsigned char* GetData() const;

	void SetMagFilter(Filter filter);
	Filter GetMagFilter() const;
	void SetMinFilter(Filter filter);
	Filter GetMinFilter() const;

	bool operator==(const Texture& second) const;
	bool operator!=(const Texture& second) const;
};