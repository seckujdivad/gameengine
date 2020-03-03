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

	scene->SetIdentifier(config["metadata"]["name"].get<std::string>());
	
	// add cameras to scene
	for (size_t i = 0; i < cameras.size(); i++)
	{
		scene->AddCamera(cameras.at(i));
	}

	// create model object library
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

		if (it.value()["merge geometry"].get<bool>())
		{
			model->MergeVertices(it.value()["merge distance"].get<GLfloat>());
		}

		model_lib[it.key()] = model;
	}

	// clone model objects into scene
	for (auto it = config["layout"].begin(); it != config["layout"].end(); it++)
	{
		model = new Model(*model_lib.at(it.value()["model"].get<std::string>()));

		model->SetPosition(it.value()["position"][0].get<GLfloat>(), it.value()["position"][1].get<GLfloat>(), it.value()["position"][2].get<GLfloat>());
		model->SetRotation(it.value()["rotation"][0].get<GLfloat>(), it.value()["rotation"][1].get<GLfloat>(), it.value()["rotation"][2].get<GLfloat>());
		model->SetScale(it.value()["scale"][0].get<GLfloat>(), it.value()["scale"][1].get<GLfloat>(), it.value()["scale"][2].get<GLfloat>());

		model->SetIdentifier(it.value()["name"].get<std::string>());

		//load texture
		wxImage image;
		image.LoadFile(path + '/' + it.value()["shader"]["textures"]["colour"].get<std::string>(), wxBITMAP_TYPE_ANY);

		unsigned char* data = image.GetData();

		//load shader
		ShaderProgram* shader_program = new ShaderProgram({
			{path + '/' + config["shaders"]["vertex"][it.value()["shader"]["vertex"].get<std::string>()].get<std::string>(), GL_VERTEX_SHADER},
			{path + '/' + config["shaders"]["fragment"][it.value()["shader"]["fragment"].get<std::string>()].get<std::string>(), GL_FRAGMENT_SHADER}
			});
		
		shader_program->LoadTexture("colourTexture", data, image.GetWidth(), image.GetHeight(), 0);

		model->SetShaderProgram(shader_program);
		scene->AddModel(model);
	}

	// delete model lib
	for (std::map<std::string, Model*>::iterator it = model_lib.begin(); it != model_lib.end(); it++)
	{
		delete it->second;
	}

	return scene;
}
