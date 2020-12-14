#include "PlyLoader.h"

#include <fstream>
#include <unordered_map>

struct PlyType
{
	bool is_list;
	bool is_ints;
};

struct PlyElement
{
	std::string name;
	int num_elements = 0;
	std::vector<PlyType> types;
	std::vector<std::string> field_names;
	std::unordered_map<std::string, int> field_name_map;
};

struct PlyValueList
{
	bool is_ints;
	std::vector<double> values_double;
	std::vector<int> values_ints;
};

bool IsPlyInt(std::string type_name)
{
	if ((type_name == "char")
		|| (type_name == "uchar")
		|| (type_name == "int")
		|| (type_name == "uint")
		|| (type_name == "int8")
		|| (type_name == "uint8")
		|| (type_name == "int16")
		|| (type_name == "uint16")
		|| (type_name == "int32")
		|| (type_name == "uint32"))
	{
		return true;
	}
	else if ((type_name == "short")
		|| (type_name == "ushort")
		|| (type_name == "float")
		|| (type_name == "double")
		|| (type_name == "float32")
		|| (type_name == "float64"))
	{
		return false;
	}
	else
	{
		throw std::invalid_argument("Not a valid float or int type");
	}
}

std::vector<std::string> SplitOnChar(std::string string, char splitter)
{
	std::vector<std::string> result;
	size_t prev_slice = 0;

	for (size_t i = 0; i < string.size(); i++)
	{
		if (string.at(i) == splitter)
		{
			std::string current_slice = string.substr(prev_slice, i - prev_slice);
			if (current_slice != "")
			{
				result.push_back(current_slice);
			}
			prev_slice = i + 1;
		}
	}

	if (prev_slice != string.size())
	{
		std::string current_slice = string.substr(prev_slice, string.size() - prev_slice);
		if (current_slice != "")
		{
			result.push_back(current_slice);
		}
	}

	return result;
}

std::vector<std::string> SplitOnChar(std::string string, std::string splitter)
{
	if (splitter.size() == 1)
	{
		return SplitOnChar(string, splitter.at(0));
	}
	else
	{
		throw std::invalid_argument("Splitter must have length 1");
	}
}

std::shared_ptr<Polygonal> ModelFromPly(std::string path)
{
	//ply files could (in theory) contain pretty much any kind of data
	//I have restricted this parser to work with Blender-style properties
	std::ifstream file; //ply filestream
	std::string line; //current line of ply file

	PlyElement* current_element = nullptr;
	std::vector<PlyElement*> header_layout;
	PlyType current_type = PlyType();

	bool in_header = true; //cursor is in header
	int element_index = -1;
	int line_index = 0; //line index in the ply file
	int pattern_index = 0; //current pattern index (post header)
	int pattern_subindex = 0; //number of elements of the current pattern that have been read (post header)
	std::vector<int> vertex_indices;

	std::vector<glm::dvec3> vertex_normals;
	std::vector<glm::dvec3> face_normals;
	std::vector<glm::dvec2> vertex_uvs;
	std::vector<glm::dvec2> face_uvs;

	std::vector<std::string> sliced_string;

	std::unordered_map<std::string, PlyValueList> values;

	std::vector<int> vertex_id_lookup;

	std::shared_ptr<Polygonal> result = std::make_shared<Polygonal>();

	file.open(path);
	if (file.is_open())
	{
		while (std::getline(file, line))
		{
			if ((line_index == 0) && (line != "ply"))
			{
				throw std::runtime_error("Invalid file format: line 1 of header should be \"ply\", not \"" + line + "\"");
			}
			else if ((line_index == 1) && (line != "format ascii 1.0"))
			{
				throw std::runtime_error("Invalid file format subtype: line 2 must be \"format ascii 1.0\", not \"" + line + "\"");
			}
			else if (line.substr(0, 8) == "comment ")
			{
				//ignore comments
			}
			else
			{
				if (in_header) //interpret data formats
				{
					if (line == "end_header")
					{
						for (int i = 0; i < (int)header_layout.size(); i++)
						{
							current_element = header_layout.at(i);
							for (int j = 0; j < (int)current_element->field_names.size(); j++)
							{
								current_element->field_name_map.insert(std::pair<std::string, int>(current_element->field_names.at(j), j));
							}
						}

						in_header = false;
					}
					else if (line.substr(0, 8) == "element ")
					{
						current_element = new PlyElement();
						sliced_string = SplitOnChar(line, ' ');

						if (sliced_string.size() == 3)
						{
							current_element->name = sliced_string.at(1);
							current_element->num_elements = std::stoi(sliced_string.at(2));
							header_layout.push_back(current_element);
						}
						else
						{
							throw std::runtime_error("Exception on line " + std::to_string(line_index + 1) + " in element definition: 3 items required, but " + std::to_string(sliced_string.size()) + " found");
						}
					}
					else if (line.substr(0, 9) == "property ")
					{
						if (current_element != nullptr)
						{
							sliced_string = SplitOnChar(line, ' ');

							if (sliced_string.size() == 3)
							{
								current_type.is_list = false;
								current_type.is_ints = IsPlyInt(sliced_string.at(1));
								current_element->field_names.push_back(sliced_string.at(2));
								current_element->types.push_back(current_type);
							}
							else if (sliced_string.size() == 5)
							{
								if (sliced_string.at(1) == "list")
								{
									current_type.is_list = true;
									current_type.is_ints = IsPlyInt(sliced_string.at(3));
									current_element->field_names.push_back(sliced_string.at(4));
									current_element->types.push_back(current_type);
								}
								else
								{
									throw std::runtime_error("Exception on line " + std::to_string(line_index + 1) + ": if there are 5 elements, the second must be \"list\"");
								}
							}
							else
							{
								throw std::runtime_error("Exception on line " + std::to_string(line_index + 1) + ": 3 or 5 items required, but " + std::to_string(sliced_string.size()) + " found");
							}
						}
					}
				}
				else //interpret body
				{
					sliced_string = SplitOnChar(line, ' ');

					if (header_layout.at(pattern_index)->name == "vertex")
					{
						vertex_id_lookup.push_back(result->AddVertex(glm::dvec3(
							std::stod(sliced_string.at(header_layout.at(pattern_index)->field_name_map.at("x"))),
							std::stod(sliced_string.at(header_layout.at(pattern_index)->field_name_map.at("y"))),
							std::stod(sliced_string.at(header_layout.at(pattern_index)->field_name_map.at("z")))
						)));
						vertex_normals.push_back(glm::dvec3(
							std::stod(sliced_string.at(header_layout.at(pattern_index)->field_name_map.at("nx"))),
							std::stod(sliced_string.at(header_layout.at(pattern_index)->field_name_map.at("ny"))),
							std::stod(sliced_string.at(header_layout.at(pattern_index)->field_name_map.at("nz")))
						));
						vertex_uvs.push_back(glm::dvec2(
							std::stod(sliced_string.at(header_layout.at(pattern_index)->field_name_map.at("s"))),
							1.0 - std::stod(sliced_string.at(header_layout.at(pattern_index)->field_name_map.at("t"))) //switch from top left (images) to bottom left (opengl) coordinate system
						));
					}
					else if (header_layout.at(pattern_index)->name == "face")
					{
						vertex_indices.clear();
						face_normals.clear();
						face_uvs.clear();

						for (int i = 1; i < std::stoi(sliced_string.at(0)) + 1; i++)
						{
							vertex_indices.push_back(std::stoi(sliced_string.at(i)));
							face_normals.push_back(vertex_normals.at(std::stoi(sliced_string.at(i))));
							face_uvs.push_back(vertex_uvs.at(std::stoi(sliced_string.at(i))));
						}

						glm::dvec3 face_normal = glm::dvec3(0.0f, 0.0f, 0.0f);
						for (size_t i = 0; i < face_normals.size(); i++)
						{
							face_normal = face_normal + face_normals.at(i);
						}

						Polygonal::Face face = Polygonal::Face(*result);
						face.SetNormal(glm::normalize(face_normal));

						for (int i = 0; i < static_cast<int>(vertex_indices.size()); i++)
						{
							Polygonal::Face::IndexedVertex vertex = Polygonal::Face::IndexedVertex(vertex_id_lookup.at(vertex_indices.at(i)), face_uvs.at(i));
							face.AddVertex(vertex);
						}

						result->AddFace(face);
					}

					//move to next subpattern
					pattern_subindex++;
					if (header_layout.at(pattern_index)->num_elements == pattern_subindex)
					{
						pattern_subindex = 0;
						pattern_index++;
					}
				}
			}

			line_index++;
		}

		//free header layout memory
		for (size_t i = 0; i < header_layout.size(); i++)
		{
			delete header_layout.at(i);
		}

		//return model
		return result;
	}
	else
	{
		throw std::invalid_argument("Can't open ply file at '" + path + "'");
	}
}

