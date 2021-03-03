#!/usr/bin/python
# -*- coding: utf-8 -*-

#to run in visual studio, press [Shift]+[Alt]+[F5]

import os
import sys

file_out = "#include \"TextureFormats.h\"\n\n#include <stdexcept>\n\n#include \"../GLTextureType.h\"\n#include \"../GLTextureFormat.h\"\n"

var_names = {
	"num channels": "num_channels",
	"pixel type": "type",
	"bit depth": "bit_depth"
	}

file_out += "\nGLint GetTextureFormatEnum(int {}, GLTextureType {}, int {})\n".format(var_names["num channels"], var_names["pixel type"], var_names["bit depth"])
file_out += "{\n\tif (false) {}"

channel_configs = ["R", "RG", "RGB", "RGBA"]

formats = {
		"({0} == GLTextureType::Float || {0} == GLTextureType::HalfFloat)": "F",
		"{0} == GLTextureType::Integer": "_SNORM",
		"{0} == GLTextureType::UnsignedByte": ""
	}

bitdepths = {
	0: "",
	4: "4",
	8: "8",
	16: "16",
	32: "32"
	}

for num_channels, num_channels_identifier in enumerate(channel_configs):
	num_channels += 1

	for bitdepth in bitdepths:
		bitdepth_identifier = bitdepths[bitdepth]

		for format_logic in formats:
			format_identifier = formats[format_logic]

			final_enum = "GL_{}{}{}".format(num_channels_identifier, bitdepth_identifier, format_identifier)

			if final_enum == "GL_R":
				final_enum = "GL_RED"

			format_logic_expanded = format_logic.format(var_names["pixel type"])
			
			condition = "else if ({} == {} && {} == {} && {})".format(var_names["num channels"], num_channels, var_names["bit depth"], bitdepth, format_logic_expanded)
			inner_code_defined = "return {};".format(final_enum)
			inner_code_undefined = "throw std::invalid_argument(\"The GLEW headers used when compiling didn't contain {}\");".format(final_enum)

			final_str = "\n\t{}\n\t{{\n#ifdef {}\n\t\t{}\n#else\n\t\t{}\n#endif\n\t}}".format(condition, final_enum, inner_code_defined, inner_code_undefined)
			
			file_out += final_str

file_out += "\n\telse\n\t{\n\t\tthrow std::invalid_argument(\"At least one argument was invalid and not caught by the wrapper function\");\n\t}\n}"

with open(os.path.join(sys.path[0], "TextureFormats.cpp"), 'w') as file:
    file.write(file_out)