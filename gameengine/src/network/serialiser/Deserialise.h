#pragma once

#include <vector>
#include <cstdint>
#include <stdexcept>
#include <string>

#include "Field.h"

namespace Serialiser
{
	template<typename T, class D>
	inline void DeserialiseValue(D& destination, std::vector<char> bytes, std::size_t& bytes_read, Field field)
	{
		if (sizeof(T) + field.offset > sizeof(D))
		{
			throw std::invalid_argument("The section of memory to be written to extends beyond the destination type");
		}

		if (field.offset < 0)
		{
			throw std::invalid_argument("Offset must be greater than or equal to zero");
		}

		if (bytes_read + sizeof(T) > bytes.size())
		{
			throw std::invalid_argument("The type provided extends past the end of the bytes to read from");
		}

		*reinterpret_cast<T*>(reinterpret_cast<char*>(&destination) + field.offset) = *reinterpret_cast<T*>(bytes.data() + bytes_read);
		bytes_read += sizeof(T);
	}

	template<class T>
	inline T Deserialise(std::vector<char> bytes, std::vector<Field> layout, std::size_t starting_byte = 0U)
	{
		T result = T();

		std::size_t bytes_read = starting_byte;
		for (const Field& field : layout)
		{
			if (field.type == Type::Int32)
			{
				DeserialiseValue<std::int32_t>(result, bytes, bytes_read, field);
			}
			else if ((field.type == Type::NullTerminatedString) || (field.type == Type::UnlimitedString))
			{
				std::string& string_target = *reinterpret_cast<std::string*>(reinterpret_cast<char*>(&result) + field.offset);
				string_target = std::string();

				bool continue_searching = bytes_read < bytes.size();
				while (continue_searching)
				{
					char c = bytes.at(bytes_read);

					if (c == '\0')
					{
						if (field.type == Type::NullTerminatedString)
						{
							continue_searching = false;
						}
						else
						{
							throw std::runtime_error("Unexpected null");
						}
					}
					else
					{
						string_target += c;
					}

					bytes_read++;

					if (bytes_read == bytes.size())
					{
						if (field.type == Type::NullTerminatedString && continue_searching)
						{
							throw std::invalid_argument("Reached end of data before finding end of null terminated string");
						}

						continue_searching = false;
					}
				}
			}
			else
			{
				throw std::invalid_argument("Unknown type: " + std::to_string(static_cast<int>(field.type)));
			}
		}

		if (bytes_read < bytes.size())
		{
			throw std::invalid_argument("The layout doesn't match the provided bytes - not all bytes were used");
		}

		return result;
	}
}