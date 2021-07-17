#include "PlyLoader.h"

#include <fstream>
#include <vector>
#include <stdexcept>
#include <tuple>
#include <variant>
#include <unordered_map>

#include "../../generic/SplitOnChar.h"

struct Property
{
	enum class Type
	{
		Double,
		Integer
	};

	inline Property(std::string name, Type type, bool is_list) : name(name), type(type), is_list(is_list) {};

	std::string name;
	Type type;
	bool is_list;
};

bool StartsWith(const std::string& string, const std::string& substring)
{
	return string.rfind(substring, 0) == 0;
}

struct Element
{
	inline Element(std::string name, int num_occurrences, std::vector<Property> properties) : name(name), num_occurrences(num_occurrences), properties(properties) {};

	std::string name;
	int num_occurrences;

	std::vector<Property> properties;

	using Parsed = std::unordered_map<std::string, std::variant<double, int, std::vector<double>, std::vector<int>>>;

	inline Parsed GetParsed(const std::vector<std::string>& tokens) const
	{
		std::unordered_map<std::string, std::variant<double, int, std::vector<double>, std::vector<int>>> result;
		std::size_t current_token = 0;
		for (const Property& property : this->properties)
		{
			if (current_token >= tokens.size())
			{
				throw std::invalid_argument("Not enough tokens to parse all properties");
			}

			if (property.is_list)
			{
				int num_elements_signed = std::stoi(tokens.at(current_token));
				if (num_elements_signed < 0)
				{
					throw std::invalid_argument("List size must be greater than or equal to zero");
				}
				std::size_t num_elements = static_cast<std::size_t>(num_elements_signed);

				std::size_t first_token = current_token + 1;
				std::size_t end_token = first_token + num_elements;
				if (end_token > tokens.size())
				{
					throw std::invalid_argument("List size requires more tokens than are provided");
				}
				else
				{
					if (property.type == Property::Type::Double)
					{
						std::vector<double> values;
						values.reserve(num_elements);
						for (std::size_t list_token = first_token; list_token < end_token; list_token++)
						{
							values.push_back(std::stod(tokens.at(list_token)));
						}
						result.insert(std::pair(property.name, values));
					}
					else if (property.type == Property::Type::Integer)
					{
						std::vector<int> values;
						values.reserve(num_elements);
						for (std::size_t list_token = first_token; list_token < end_token; list_token++)
						{
							values.push_back(std::stoi(tokens.at(list_token)));
						}
						result.insert(std::pair(property.name, values));
					}
					else
					{
						throw std::runtime_error("Unknown property type");
					}
					
					current_token = end_token; //move to first token after the list
				}
			}
			else
			{
				if (property.type == Property::Type::Double)
				{
					result.insert(std::pair(property.name, std::stod(tokens.at(current_token))));
				}
				else if (property.type == Property::Type::Integer)
				{
					result.insert(std::pair(property.name, std::stoi(tokens.at(current_token))));
				}
				else
				{
					throw std::runtime_error("Unknown property type");
				}

				current_token++;
			}
		}

		if (current_token < tokens.size())
		{
			throw std::invalid_argument("Too many tokens provided");
		}

		return result;
	}
};

Property::Type TranslatePlyType(std::string type_name)
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
		return Property::Type::Integer;
	}
	else if ((type_name == "short")
		|| (type_name == "ushort")
		|| (type_name == "float")
		|| (type_name == "double")
		|| (type_name == "float32")
		|| (type_name == "float64"))
	{
		return Property::Type::Double;
	}
	else
	{
		throw std::invalid_argument("Not a valid float or int type");
	}
}

std::shared_ptr<Polygonal> ModelFromPlyText(const std::string& text)
{
	return ModelFromPlyText(SplitOnLineEnd(text));
}

std::shared_ptr<Polygonal> ModelFromPlyText(const std::vector<std::string>& text)
{
	//remove all comments and blank lines
	std::vector<std::string> text_filtered;
	text_filtered.reserve(text.size()); //grab a bigger allocation than necessary and (then shrink it after)
	for (const std::string& line : text)
	{
		if (!line.empty() && !StartsWith(line, "comment "))
		{
			text_filtered.push_back(line);
		}
	}
	text_filtered.shrink_to_fit();

	if (text_filtered.size() < 2)
	{
		throw std::invalid_argument("Header must start with two lines of format identification information");
	}

	//check initial file format info
	if (text_filtered.at(0) != "ply")
	{
		throw std::invalid_argument("First line must be \"ply\"");
	}

	if (text_filtered.at(1) != "format ascii 1.0")
	{
		throw std::invalid_argument("Second line must be \"format ascii 1.0\"");
	}

	//process header
	bool end_of_header_found = false;
	std::vector<Element> elements;
	std::size_t current_line = 2;
	for (current_line = 2; (current_line < text_filtered.size()) && !end_of_header_found; current_line++)
	{
		const std::string& line = text_filtered.at(current_line);
		if (line == "end_header")
		{
			end_of_header_found = true;
		}
		else
		{
			std::vector<std::string> split_line = SplitOnChar(line, ' ');
			if (split_line.size() == 3 || split_line.size() == 5)
			{
				if (split_line.at(0) == "element")
				{
					if (split_line.size() == 3)
					{
						elements.push_back(Element(split_line.at(1), std::stoi(split_line.at(2)), {}));
					}
					else
					{
						throw std::invalid_argument("\"element\" declaration must contain exactly 3 tokens");
					}
				}
				else if (split_line.at(0) == "property")
				{
					if (elements.size() == 0)
					{
						throw std::invalid_argument("\"property\" declaration must not appear before the first \"element\" declaration");
					}
					else
					{
						Element& element = *elements.rbegin();
						if (split_line.size() == 3) //single element property
						{
							element.properties.push_back(Property(split_line.at(2), TranslatePlyType(split_line.at(1)), false));
						}
						else if (split_line.size() == 5) //list property
						{
							if (split_line.at(1) == "list")
							{
								if (TranslatePlyType(split_line.at(2)) == Property::Type::Integer)
								{
									element.properties.push_back(Property(split_line.at(4), TranslatePlyType(split_line.at(3)), true));
								}
								else
								{
									throw std::invalid_argument("List property size type must be an integer-like");
								}
							}
							else
							{
								throw std::invalid_argument("\"property\" declarations with 5 tokens must be declaring list properties");
							}
						}
						else
						{
							throw std::invalid_argument("\"property\" declaration must contain exactly 3 or 5 tokens");
						}
					}
				}
				else
				{
					throw std::invalid_argument("Unknown leading token \"" + split_line.at(0) + "\"");
				}
			}
			else
			{
				throw std::invalid_argument("Wrong number of tokens on line");
			}
		}
	}

	if (end_of_header_found)
	{
		//process body
		std::unordered_map<std::string, std::vector<Element::Parsed>> parsed_elements;
		for (const Element& element : elements)
		{
			parsed_elements.insert(std::pair(element.name, std::vector<Element::Parsed>({})));
			
			for (int i = 0; i < element.num_occurrences; i++)
			{
				const std::string& line = text_filtered.at(current_line);
				std::vector<std::string> tokens = SplitOnChar(line, ' ');

				parsed_elements.at(element.name).push_back(element.GetParsed(tokens));

				current_line++;
			}
		}

		if (current_line < text_filtered.size())
		{
			throw std::invalid_argument("Too many lines provided");
		}
		else
		{
			//turn loaded data into Polygonal object - this is the only part that is not specific to parsing the file format itself
			std::shared_ptr<Polygonal> polygonal = std::make_shared<Polygonal>();

			//load vertices
			std::unordered_map<std::size_t, int> vertex_indices; //key is the file vertex index, value is the Polygonal-returned index
			for (std::size_t i = 0; i < parsed_elements.at("vertex").size(); i++)
			{
				const Element::Parsed& vertex = parsed_elements.at("vertex").at(i);
				glm::dvec3 vertex_position = glm::dvec3(
					std::get<double>(vertex.at("x")),
					std::get<double>(vertex.at("y")),
					std::get<double>(vertex.at("z"))
				);
				vertex_indices.insert(std::pair(i, polygonal->AddVertex(vertex_position)));
			}

			//load faces
			for (const Element::Parsed& parsed_element : parsed_elements.at("face"))
			{
				Polygonal::Face face = Polygonal::Face(*polygonal);

				std::vector<glm::dvec3> normals;
				for (int vertex_index : std::get<std::vector<int>>(parsed_element.at("vertex_indices")))
				{
					const Element::Parsed& vertex = parsed_elements.at("vertex").at(vertex_index);

					//the UV properties aren't outputted by Blender if the model hasn't been unwrapped, so provide a default value if they don't exist
					glm::dvec2 uv = glm::dvec2(0.0);
					if (vertex.count("s") > 0 && vertex.count("t") > 0)
					{
						uv.x = std::get<double>(vertex.at("s"));
						uv.y = 1.0 - std::get<double>(vertex.at("t"));

						/*
						* Blender (and maybe others) exports UVs with the origin in the top left, however OpenGL
						* has the origin in the bottom left
						*/
					}

					normals.push_back(glm::dvec3(
						std::get<double>(vertex.at("nx")),
						std::get<double>(vertex.at("ny")),
						std::get<double>(vertex.at("nz"))
					));

					face.AddVertex(Polygonal::Face::IndexedVertex(vertex_indices.at(vertex_index), uv));
				}

				glm::dvec3 normal = glm::dvec3(0.0);
				for (const glm::dvec3 vertex_normal : normals)
				{
					normal += vertex_normal;
				}
				face.SetNormal(glm::normalize(normal));

				polygonal->AddFace(face);
			}

			return polygonal;
		}
	}
	else
	{
		throw std::invalid_argument("Never found line end_header - the header must end in the file");
	}
}

std::shared_ptr<Polygonal> ModelFromPly(std::string path)
{
	std::ifstream file;
	std::vector<std::string> file_contents;

	file.open(path);
	if (file.is_open())
	{
		std::string line;
		while (std::getline(file, line))
		{
			file_contents.push_back(line);
		}
	}
	else
	{
		throw std::invalid_argument("Can't open ply file at '" + path + "'");
	}

	return ModelFromPlyText(file_contents);
}