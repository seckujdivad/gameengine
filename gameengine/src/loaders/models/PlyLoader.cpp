#include "PlyLoader.h"

#include <fstream>
#include <stdexcept>
#include <variant>
#include <unordered_map>

#include <glm/glm.hpp>

#include "../../generic/SplitOnChar.h"
#include "PLYData.h"
#include "../../scene/model/geometry/Polygonal.h"

std::shared_ptr<Polygonal> ModelFromPlyText(const std::string& text)
{
	return ModelFromPlyText(SplitOnLineEnd(text));
}

std::shared_ptr<Polygonal> ModelFromPlyText(const std::vector<std::string>& text)
{
	PLYData parsed_elements = ParsePLYFile(text);
	
	//turn loaded data into Polygonal object - this is the only part that is not specific to parsing the file format itself
	std::shared_ptr<Polygonal> polygonal = std::make_shared<Polygonal>();

	//load vertices
	std::unordered_map<std::size_t, int> vertex_indices; //key is the file vertex index, value is the Polygonal-returned index
	for (std::size_t i = 0; i < parsed_elements.at("vertex").size(); i++)
	{
		const ParsedElement& vertex = parsed_elements.at("vertex").at(i);
		glm::dvec3 vertex_position = glm::dvec3(
			std::get<double>(vertex.at("x")),
			std::get<double>(vertex.at("y")),
			std::get<double>(vertex.at("z"))
		);
		vertex_indices.insert(std::pair(i, polygonal->AddVertex(vertex_position)));
	}

	//load faces
	for (const ParsedElement& parsed_element : parsed_elements.at("face"))
	{
		Polygonal::Face face = Polygonal::Face(*polygonal);

		std::vector<glm::dvec3> normals;
		for (int vertex_index : std::get<std::vector<int>>(parsed_element.at("vertex_indices")))
		{
			const ParsedElement& vertex = parsed_elements.at("vertex").at(vertex_index);

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