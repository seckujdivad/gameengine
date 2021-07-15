#pragma once

#include <glm/glm.hpp>

#include <tuple>
#include <vector>
#include <array>

#include "../Referenceable.h"
#include "../RevisableResource.h"

enum class TextureFiltering;

class Texture : public Referenceable<TextureReference>, public RevisableResource
{
public:
	enum class Type
	{
		None,
		FullTexture,
		Vector
	};

private:
	Type m_type = Type::None;
	TextureFiltering m_filter_mag;
	TextureFiltering m_filter_min;

	std::vector<unsigned char> m_data;

	//full texture
	std::tuple<int, int> m_dimensions = { -1, -1 };

	//vector
	glm::vec3 m_vec_colour = glm::vec3(0.0f);

	static unsigned char NormalisedFloatToByte(float value);
	static float ByteToNormalisedFloat(unsigned char value);

	static std::array<unsigned char, 3> VecToBytes(glm::vec3 vec);
	static glm::vec3 BytesToVec(std::array<unsigned char, 3> arr);

public:
	Texture(TextureReference reference);
	Texture(TextureReference reference, glm::vec3 colour);

	Type GetType() const;

	//vectors
	void SetVector(glm::vec3 colour);
	glm::vec3 GetVector() const;

	//full textures
	void SetFullTexture(const unsigned char* data, std::tuple<int, int> dimensions);
	void SetFullTexture(std::vector<glm::vec3> colour, std::tuple<int, int> dimensions);
	void SetFullTexture(glm::vec3 colour, std::tuple<int, int> dimensions);

	std::size_t GetPixelIndex(int x, int y) const;

	glm::vec3 GetPixel(int x, int y) const;
	void SetPixel(int x, int y, glm::vec3 value);

	//generic access
	std::tuple<int, int> GetDimensions() const;
	const unsigned char* GetData() const;

	//generic filtering modes
	void SetMagFilter(TextureFiltering filter);
	TextureFiltering GetMagFilter() const;
	void SetMinFilter(TextureFiltering filter);
	TextureFiltering GetMinFilter() const;

	//generic comparison operators
	bool operator==(const Texture& second) const;
	bool operator!=(const Texture& second) const;
};