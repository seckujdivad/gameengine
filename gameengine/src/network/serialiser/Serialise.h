#pragma once

#include <vector>
#include <string>

#include <glm/glm.hpp>

#include "Field.h"

namespace Serialiser
{
	template<typename Property, class Source>
	inline const Property& GetProperty(const Source& source, std::size_t offset)
	{
		return *reinterpret_cast<const Property*>(reinterpret_cast<const char*>(&source) + offset);
	}

	template<class Source>
	inline std::vector<char> Serialise(const Source& source, std::vector<Field> layout)
	{
		std::vector<char> result;
		for (const Field& field : layout)
		{
			if (field.type == Type::Int32)
			{
				for (std::size_t i = 0; i < sizeof(int32_t); i++)
				{
					result.push_back(*(reinterpret_cast<const char*>(&source) + field.offset + i));
				}
			}
			else if ((field.type == Type::NullTerminatedString) || (field.type == Type::UnlimitedString))
			{
				const std::string& target_string = GetProperty<std::string>(source, field.offset);
				for (char c : target_string)
				{
					result.push_back(c);
				}

				if (field.type == Type::NullTerminatedString)
				{
					result.push_back('\0');
				}
			}
			else if (field.type == Type::DoubleVec3)
			{
				const glm::dvec3& vec = GetProperty<glm::dvec3>(source, field.offset);
				for (int dimension = 0; dimension < 3; dimension++)
				{
					const char* dimension_value = reinterpret_cast<const char*>(&vec[dimension]);
					for (std::size_t byte = 0; byte < sizeof(double); byte++)
					{
						result.push_back(dimension_value[byte]);
					}
				}
			}
		}

		return result;
	}
}