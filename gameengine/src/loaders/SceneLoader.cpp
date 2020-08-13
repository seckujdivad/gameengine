#include "SceneLoader.h"

Scene* SceneFromJSON(std::filesystem::path root_path, std::filesystem::path file_name)
{
	//load json
	nlohmann::json scene_data;
	{
		std::ifstream file_stream;
		std::string file_contents;
		std::string line_contents;

		std::filesystem::path json_path = root_path;
		json_path += file_name;

		file_stream.open(json_path);
		if (file_stream.is_open())
		{
			while (std::getline(file_stream, line_contents))
			{
				file_contents = file_contents + line_contents + '\n';
			}
		}
		else
		{
			throw std::invalid_argument("Can't open file at " + root_path.string());
		}

		scene_data = nlohmann::json::parse(file_contents);
	}

	Scene* scene = new Scene();

	return scene;
}

Scene* SceneFromJSON(std::string root_path, std::string file_name)
{
	return SceneFromJSON(std::filesystem::path(root_path), std::filesystem::path(file_name));
}
