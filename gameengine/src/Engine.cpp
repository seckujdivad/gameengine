#include <wx/wxprec.h>
#include "Engine.h"

Scene* InitialiseScene(std::string path, std::string filename)
{
	//load config
	std::ifstream file;
	std::string contents;
	std::string line;

	file.open(path + "/" +  filename);
	if (file.is_open())
	{
		while (std::getline(file, line))
		{
			contents = contents + line + "\n";
		}
	}
	else
	{
		throw std::invalid_argument("Can't open file at '" + path + "/" + filename + "'");
	}

	json config = json::parse(contents);

	//create scene
	// make cameras
	std::vector<Camera*> cameras;
	Camera* camera;
	Camera* main_camera = nullptr;
	for (auto it = config["cameras"].begin(); it != config["cameras"].end(); it++)
	{
		camera = new Camera();
		camera->SetIdentifier(it.key());
		camera->SetPosition(it.value()["position"][0].get<GLfloat>(), it.value()["position"][1].get<GLfloat>(), it.value()["position"][2].get<GLfloat>());
		camera->SetRotation(it.value()["rotation"][0].get<GLfloat>(), it.value()["rotation"][1].get<GLfloat>(), it.value()["rotation"][2].get<GLfloat>());
		camera->SetFOV(it.value()["fov"].get<GLfloat>());
		cameras.push_back(camera);

		if (it.key() == config["metadata"]["default camera"].get<std::string>())
		{
			main_camera = camera;
		}
	}

	if (main_camera == nullptr)
	{
		if (cameras.size() == 0)
		{
			throw "At least one camera must be defined";
		}
		main_camera = cameras.at(0);
	}

	// make scene object
	Scene* scene = new Scene(main_camera);
	
	// add cameras to scene
	for (size_t i = 0; i < cameras.size(); i++)
	{
		scene->AddCamera(cameras.at(i));
	}

	// create model object
	std::map<std::string, Model*> model_lib;
	Model* model = nullptr;
	for (auto it = config["models"].begin(); it != config["models"].end(); it++)
	{
		if (it.value()["type"] == "ply")
		{
			std::string full_path = path + "/";
			full_path = full_path + it.value()["geometry"].get<std::string>();
			model = ModelFromPly(full_path);
		}

		model_lib[it.key()] = model;
	}

	// clone model objects into scene
	for (auto it = config["layout"].begin(); it != config["layout"].end(); it++)
	{
		model = new Model(model_lib.at(it.value()["model"].get<std::string>()));

		model->SetPosition(it.value()["position"][0].get<GLfloat>(), it.value()["position"][1].get<GLfloat>(), it.value()["position"][2].get<GLfloat>());
		model->SetRotation(it.value()["rotation"][0].get<GLfloat>(), it.value()["rotation"][1].get<GLfloat>(), it.value()["rotation"][2].get<GLfloat>());
		model->SetScale(it.value()["scale"][0].get<GLfloat>(), it.value()["scale"][1].get<GLfloat>(), it.value()["scale"][2].get<GLfloat>());

		model->SetIdentifier(it.value()["name"].get<std::string>());

		scene->AddModel(model);
	}

	// load shaders

	// delete model lib
	for (std::map<std::string, Model*>::iterator it = model_lib.begin(); it != model_lib.end(); it++)
	{
		delete it->second;
	}

	return scene;
}

Model* ModelFromPly(std::string path)
{
	//ply files could (in theory) contain pretty much any kind of data
	//I have restricted this parser to work with Blender-style properties
	std::ifstream file; //ply filestream
	std::string line; //current line of ply file

	PlyElement* current_element = nullptr;
	std::vector<PlyElement*> header_layout;
	PlyType current_type;

	bool in_header = true; //cursor is in header
	int element_index = -1;
	int line_index = 0; //line index in the ply file
	int pattern_index = 0; //current pattern index (post header)
	int pattern_subindex = 0; //number of elements of the current pattern that have been read (post header)

	std::vector<std::string> sliced_string;

	std::map<std::string, PlyValueList> values;

	Model* result = new Model();

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
							current_element->num_elements++;
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

					for (size_t i = 0; i < sliced_string.size(); )
					{

						i++;
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
		throw std::runtime_error("Not a valid float or int type");
	}
}

std::vector<std::string> SplitOnChar(std::string string, char splitter, bool add_empty)
{
	std::vector<std::string> result;
	std::string current_slice = "";

	for (size_t i = 0; i < string.size(); i++)
	{
		if (string.at(i) == splitter)
		{
			if ((current_slice != "") || (add_empty))
			{
				result.push_back(current_slice);
				current_slice = "";
			}
		}
		else
		{
			current_slice = current_slice + string.at(i);
		}
	}

	if (current_slice != "")
	{
		result.push_back(current_slice);
	}

	return result;
}

std::vector<std::string> SplitOnChar(std::string string, std::string splitter, bool add_empty)
{
	if (splitter.size() == 1)
	{
		return SplitOnChar(string, splitter.at(0), add_empty);
	}
	else
	{
		throw std::runtime_error("Splitter must have length 1");
	}
}

template<typename T>
int FindInVector(std::vector<T> to_search, T search_item)
{
	for (size_t i = 0; i < to_search.size(); i++)
	{
		if (to_search.at(i) == search_item)
		{
			return i;
		}
	}
	return -1;
}