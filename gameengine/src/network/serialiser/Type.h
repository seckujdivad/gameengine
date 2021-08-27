#pragma once

/*
* When adding a new type:
* 1. add a new enum
* 2. add to Serialiser::Serialise
* 3. add to Serialiser::Deserialise
*/

namespace Serialiser
{
	enum class Type
	{
		//integral types
		Int32,

		//vector types
		DoubleVec3,

		//string types - these expect to write into a std::string
		NullTerminatedString,
		UnlimitedString //a string that ends at the end of the data
	};
}