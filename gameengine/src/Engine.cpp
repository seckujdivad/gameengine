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

	//load lighting
	scene->SetAmbientLight(glm::vec3(config["lighting"]["ambient"][0].get<float>(), config["lighting"]["ambient"][1].get<float>(), config["lighting"]["ambient"][2].get<float>()));

	int num_point_lights = 0;
	for (auto it = config["lighting"]["point lights"].begin(); it != config["lighting"]["point lights"].end(); it++)
	{
		PointLight* pointlight = new PointLight(num_point_lights);
		pointlight->SetIdentifier(it.value()["name"].get<std::string>());
		pointlight->SetIntensity(glm::vec3(it.value()["intensity"][0].get<float>(),
			it.value()["intensity"][1].get<float>(),
			it.value()["intensity"][2].get<float>()));
		pointlight->SetPosition(0, it.value()["position"][0].get<float>());
		pointlight->SetPosition(1, it.value()["position"][1].get<float>());
		pointlight->SetPosition(2, it.value()["position"][2].get<float>());

		if (it.value()["shadows"].get<bool>())
		{
			pointlight->EnableShadows(it.value()["shadow texture"][0].get<unsigned int>(), it.value()["shadow texture"][1].get<unsigned int>(),
				it.value()["shadow clips"][0].get<float>(), it.value()["shadow clips"][1].get<float>());
		}

		scene->AddPointLight(pointlight);

		num_point_lights++;
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
	ShaderProgram* shader_program;
	for (auto it = config["layout"].begin(); it != config["layout"].end(); it++)
	{
		model = new Model(*model_lib.at(it.value()["model"].get<std::string>()));

		model->SetPosition(it.value()["position"][0].get<GLfloat>(), it.value()["position"][1].get<GLfloat>(), it.value()["position"][2].get<GLfloat>());
		model->SetRotation(it.value()["rotation"][0].get<GLfloat>(), it.value()["rotation"][1].get<GLfloat>(), it.value()["rotation"][2].get<GLfloat>());
		model->SetScale(it.value()["scale"][0].get<GLfloat>(), it.value()["scale"][1].get<GLfloat>(), it.value()["scale"][2].get<GLfloat>());

		model->SetIdentifier(it.value()["name"].get<std::string>());

		//load shader
		shader_program = new ShaderProgram(GetShaders(path, config, it.value()["shader"]["render"]),
			{
				{"POINT_LIGHT_NUM", std::to_string(num_point_lights)}
			});
		
		Material mat;
		mat.SetDiffuse(glm::vec3(it.value()["shader"]["phong"]["diffuse"][0].get<float>(),
			it.value()["shader"]["phong"]["diffuse"][1].get<float>(),
			it.value()["shader"]["phong"]["diffuse"][2].get<float>()));
		mat.SetSpecular(glm::vec3(it.value()["shader"]["phong"]["specular"][0].get<float>(),
			it.value()["shader"]["phong"]["specular"][1].get<float>(),
			it.value()["shader"]["phong"]["specular"][2].get<float>()));
		mat.SetSpecularHighlight(it.value()["shader"]["phong"]["specular highlight"].get<float>());

		model->SetMaterial(mat);

		// load texture
		wxImage image;
		image.LoadFile(path + '/' + it.value()["shader"]["textures"]["colour"].get<std::string>(), wxBITMAP_TYPE_ANY);

		unsigned char* data = image.GetData();
		
		shader_program->LoadTexture("colourTexture", data, image.GetWidth(), image.GetHeight(), 0);

		// store shader program
		model->SetShaderProgram(shader_program);

		//load shadow shader
		shader_program = new ShaderProgram(GetShaders(path, config, it.value()["shader"]["shadow"]), {});
		model->SetShadowShaderProgram(shader_program);

		//store model
		scene->AddModel(model);
	}

	// delete model lib
	for (std::map<std::string, Model*>::iterator it = model_lib.begin(); it != model_lib.end(); it++)
	{
		delete it->second;
	}

	return scene;
}

std::vector<std::tuple<std::string, GLenum>> GetShaders(std::string base_path, nlohmann::json config, nlohmann::basic_json<> shader_config)
{
	std::vector<std::tuple<std::string, GLenum>> output = {};

	int shader_type;
	int j = shader_config.size();
	for (auto it = shader_config.begin(); it != shader_config.end(); it++)
	{
		if (it.key() == "vertex")
		{
			shader_type = GL_VERTEX_SHADER;
		}
		else if (it.key() == "fragment")
		{
			shader_type = GL_FRAGMENT_SHADER;
		}
		else if (it.key() == "geometry")
		{
			shader_type = GL_GEOMETRY_SHADER;
		}
		else
		{
			throw std::runtime_error("Can't find shader type " + it.value().get<std::string>());
		}
		output.push_back({base_path + "/" + config["shaders"][it.key()][shader_config[it.key()].get<std::string>()].get<std::string>(), shader_type});
	}

	return output;
}