#pragma once

#include <glm/glm.hpp>

#include <tuple>
#include <stdexcept>
#include <string>

#include "Referenceable.h"

enum class LocalTextureType
{
	None,
	FullTexture,
	Vector
};

enum class LocalTextureFilter
{
	Nearest,
	Linear
};

class LocalTexture : public Referenceable<TextureReference>
{
private:
	LocalTextureType m_type = LocalTextureType::None;
	LocalTextureFilter m_filter_mag = LocalTextureFilter::Nearest;
	LocalTextureFilter m_filter_min = LocalTextureFilter::Nearest;

	//full texture
	unsigned char* m_full_data = nullptr;
	std::tuple<int, int> m_dimensions = { -1, -1 };

	//vector
	glm::vec3 m_vec_colour = glm::vec3(0.0f);
	unsigned char* m_vec_data = nullptr;

public:
	LocalTexture(TextureReference reference);
	LocalTexture(const LocalTexture& copy_from);
	~LocalTexture();

	void SetVector(glm::vec3 colour);

	void SetFullTexture(unsigned char* data, std::tuple<int, int> dimensions);

	std::tuple<int, int> GetDimensions();
	unsigned char* GetData();

	void SetMagFilter(LocalTextureFilter filter);
	LocalTextureFilter GetMagFilter();
	void SetMinFilter(LocalTextureFilter filter);
	LocalTextureFilter GetMinFilter();
};