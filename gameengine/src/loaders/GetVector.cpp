#include "GetVector.h"

template<unsigned int dimensions, typename T = double>
vec<dimensions, T> GetVector(const nlohmann::json& data, vec<dimensions, T> default_value)
{
	static_assert(dimensions > 0, "Dimensions must be greater than zero");
	static_assert(dimensions < 5, "Dimensions must be 4 or below");

	if (data.is_array())
	{
		if (data.size() == dimensions)
		{
			bool is_nums = true;
			for (unsigned int i = 0; i < dimensions; i++)
			{
				if (!data.at(i).is_number())
				{
					is_nums = false;
				}
			}

			if (is_nums)
			{
				std::vector<T> values;
				for (auto& el : data.items())
				{
					values.push_back(el.value().get<T>());
				}

				vec<dimensions, T> result = default_value;
				for (int i = 0; i < static_cast<int>(std::min(static_cast<unsigned int>(values.size()), dimensions)); i++)
				{
					result[i] = values.at(i);
				}

				return result;
			}
			else
			{
				throw std::runtime_error("All values in a vector must be numbers");
			}
		}
		else
		{
			throw std::runtime_error("Dimensions (" + std::to_string(dimensions) + ") are not equal to the number of given values (" + std::to_string(data.size()) + ")");
		}
	}
	else if (data.is_number())
	{
		return vec<dimensions, T>(data.get<T>());
	}
	else
	{
		return default_value;
	}
}

template glm::vec1 GetVector(const nlohmann::json&, glm::vec1);
template glm::vec2 GetVector(const nlohmann::json&, glm::vec2);
template glm::vec3 GetVector(const nlohmann::json&, glm::vec3);
template glm::vec4 GetVector(const nlohmann::json&, glm::vec4);
template glm::dvec1 GetVector(const nlohmann::json&, glm::dvec1);
template glm::dvec2 GetVector(const nlohmann::json&, glm::dvec2);
template glm::dvec3 GetVector(const nlohmann::json&, glm::dvec3);
template glm::dvec4 GetVector(const nlohmann::json&, glm::dvec4);
template glm::ivec1 GetVector(const nlohmann::json&, glm::ivec1);
template glm::ivec2 GetVector(const nlohmann::json&, glm::ivec2);
template glm::ivec3 GetVector(const nlohmann::json&, glm::ivec3);
template glm::ivec4 GetVector(const nlohmann::json&, glm::ivec4);