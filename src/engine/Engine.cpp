#include "Engine.h"

Scene* InitialiseScene(std::string path)
{
	//load config
	std::ifstream file;
	std::string contents;
	std::string line;

	file.open(path);
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
		main_camera = cameras.at(0);
	}

	// make scene object
	Scene* scene = new Scene(main_camera);

	// create model object

	// clone model objects into scene

	// load shaders

	return scene;
}