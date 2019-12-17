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
		throw std::invalid_argument("Can't open file at '" + path + "'");
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
		camera->SetPosition((GLfloat)it.value()["position"][0], (GLfloat)it.value()["position"][1], (GLfloat)it.value()["position"][2]);
		camera->SetRotation((GLfloat)it.value()["rotation"][0], (GLfloat)it.value()["rotation"][1], (GLfloat)it.value()["rotation"][2]);
		camera->SetFOV((GLfloat)it.value()["fov"]);
		cameras.push_back(camera);

		if (it.key() == config["metadata"]["default camera"])
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

	// create model object
	std::map<std::string, Model*> model_lib;
	Model* model = nullptr;
	for (auto it = config["models"].begin(); it != config["models"].end(); it++)
	{
		if (it.value()["type"] == "ply")
		{
			std::string full_path = path + "/";
			full_path = full_path + it.value()["geometry"];
			model = ModelFromPly(full_path);
		}

		model_lib[it.key()] = model;
	}

	// clone model objects into scene
	for (auto it = config["layout"].begin(); it != config["layout"].end(); it++)
	{
		model = new Model(model_lib.at(it.value()["model"]));

		model->SetPosition((GLfloat)it.value()["position"][0], (GLfloat)it.value()["position"][1], (GLfloat)it.value()["position"][2]);
		model->SetRotation((GLfloat)it.value()["rotation"][0], (GLfloat)it.value()["rotation"][1], (GLfloat)it.value()["rotation"][2]);
		model->SetScale((GLfloat)it.value()["scale"][0], (GLfloat)it.value()["scale"][1], (GLfloat)it.value()["scale"][2]);

		model->SetIdentifier(it.value()["name"]);

		scene->AddModel(model);
	}

	// load shaders

	return scene;
}

Model* ModelFromPly(std::string path)
{
	return nullptr;
}