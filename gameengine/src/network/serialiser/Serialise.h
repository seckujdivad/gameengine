#pragma once

#include <vector>
#include <string>

#include "Field.h"

namespace Serialiser
{
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
				const std::string& target_string = *reinterpret_cast<const std::string*>(reinterpret_cast<const char*>(&source) + field.offset);
				for (char c : target_string)
				{
					result.push_back(c);
				}

				if (field.type == Type::NullTerminatedString)
				{
					result.push_back('\0');
				}
			}
		}

		return result;
	}
}