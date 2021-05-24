#pragma once

#include <cstddef>

#include "Type.h"

namespace Serialiser
{
	struct Field
	{
		constexpr Field(Type type, std::size_t offset) : type(type), offset(offset) {};

		Type type;
		std::size_t offset;
	};
}