#pragma once

namespace Serialiser
{
	enum class Type
	{
		Int32,

		//string types - these expect to write into a std::string
		NullTerminatedString,
		UnlimitedString //a string that ends at the end of the data
	};
}