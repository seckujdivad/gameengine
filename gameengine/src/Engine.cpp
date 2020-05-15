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
		camera->SetNearClip(it.value()["clips"][0].get<GLfloat>());
		camera->SetFarClip(it.value()["clips"][1].get<GLfloat>());
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
		PointLight* pointlight = new PointLight(num_point_lights, it.value()["dynamic refresh rate"].get<int>());
		pointlight->SetIdentifier(it.key());
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

		for (auto model_it = it.value()["static draw"].begin(); model_it != it.value()["static draw"].end(); model_it++)
		{
			pointlight->AddStaticModel(model_it.value().get<std::string>());
		}

		for (auto model_it = it.value()["dynamic draw"].begin(); model_it != it.value()["dynamic draw"].end(); model_it++)
		{
			pointlight->AddDynamicModel(model_it.value().get<std::string>());
		}

		scene->AddPointLight(pointlight);

		num_point_lights++;
	}
	
	//load default screen space reflection values
	MaterialSSRConfig default_ssr;
	default_ssr.resolution = config["screen space reflection defaults"]["resolution"].get<float>();
	default_ssr.cast_distance_limit = config["screen space reflection defaults"]["distance limit"].get<float>();
	default_ssr.depth_acceptance = config["screen space reflection defaults"]["acceptable depth distance"].get<float>();
	default_ssr.max_cam_distance = config["screen space reflection defaults"]["max camera distance"].get<float>();
	default_ssr.appear_in_ssr = config["screen space reflection defaults"]["appear in ssr"].get<bool>();
	default_ssr.refinements = config["screen space reflection defaults"]["refinements"].get<int>();

	//load reflections
	Reflection* reflection;
	OrientedBoundingBox obb;
	for (auto it = config["reflections"].begin(); it != config["reflections"].end(); it++)
	{
		reflection = new Reflection(it.value()["texture"][0].get<int>(),
			it.value()["texture"][1].get<int>(),
			it.value()["clips"][0].get<float>(),
			it.value()["clips"][1].get<float>(),
			it.value()["dynamic refresh rate"].get<int>());

		reflection->SetIdentifier(it.key());
		reflection->SetPosition(it.value()["position"][0].get<float>(),
			it.value()["position"][1].get<float>(),
			it.value()["position"][2].get<float>());

		reflection->ConfigureIterative(it.value()["corrections"]["iterative sampling"]["iterations"].get<int>());

		for (auto model_it = it.value()["static draw"].begin(); model_it != it.value()["static draw"].end(); model_it++)
		{
			reflection->AddStaticModel(model_it.value().get<std::string>());
		}

		for (auto model_it = it.value()["dynamic draw"].begin(); model_it != it.value()["dynamic draw"].end(); model_it++)
		{
			reflection->AddDynamicModel(model_it.value().get<std::string>());
		}

		scene->AddReflection(reflection);
	}

	//load vis boxes
	std::map<std::string, VisBox*> visboxes;
	std::map<VisBox*, std::vector<std::string>> visbox_pvs;
	for (auto it = config["vis boxes"].begin(); it != config["vis boxes"].end(); it++)
	{
		VisBox* visbox = new VisBox();

		visbox->SetIdentifier(it.key());
		visbox->SetPosition(it.value()["position"][0].get<float>(),
			it.value()["position"][1].get<float>(),
			it.value()["position"][2].get<float>());
		visbox->SetRotation(it.value()["rotation"][0].get<float>(),
			it.value()["rotation"][1].get<float>(),
			it.value()["rotation"][2].get<float>());
		visbox->SetScale(it.value()["dimensions"][0].get<float>() / 2.0f,
			it.value()["dimensions"][1].get<float>() / 2.0f,
			it.value()["dimensions"][2].get<float>() / 2.0f);

		visboxes.insert({ it.key(), visbox });

		std::vector<std::string> pvs;
		for (auto itb = it.value()["pvs"].begin(); itb != it.value()["pvs"].end(); itb++)
		{
			pvs.push_back(itb.value().get<std::string>());
		}
		visbox_pvs.insert({ visbox, pvs });

		scene->AddVisBox(visbox);
	}

	//load vis box potentially visible sets
	for (auto it = visbox_pvs.begin(); it != visbox_pvs.end(); it++)
	{
		for (size_t i = 0; i < it->second.size(); i++)
		{
			it->first->AddPotentiallyVisible(visboxes.at(it->second.at(i)));
		}
	}

	//load obb scene approximation
	SceneApproximation* approximation = new SceneApproximation();
	for (auto it = config["obb approximation"].begin(); it != config["obb approximation"].end(); it++)
	{
		obb.SetPosition(glm::vec3(it.value()["position"][0].get<float>(),
			it.value()["position"][1].get<float>(),
			it.value()["position"][2].get<float>()));
		obb.SetRotation(glm::vec3(it.value()["rotation"][0].get<float>(),
			it.value()["rotation"][1].get<float>(),
			it.value()["rotation"][2].get<float>()));
		obb.SetScale(glm::vec3(it.value()["dimensions"][0].get<float>(),
			it.value()["dimensions"][1].get<float>(),
			it.value()["dimensions"][2].get<float>()) * 0.5f);
		approximation->AddOBB(obb);
	}
	scene->SetApproximation(approximation);
	
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
	int reflection_mode;
	ShaderDescription shader_description;
	int num_reflections;
	for (auto it = config["layout"].begin(); it != config["layout"].end(); it++)
	{
		model = new Model(*model_lib.at(it.value()["model"].get<std::string>()));

		model->SetPosition(it.value()["position"][0].get<GLfloat>(), it.value()["position"][1].get<GLfloat>(), it.value()["position"][2].get<GLfloat>());
		model->SetRotation(it.value()["rotation"][0].get<GLfloat>(), it.value()["rotation"][1].get<GLfloat>(), it.value()["rotation"][2].get<GLfloat>());
		model->SetScale(it.value()["scale"][0].get<GLfloat>(), it.value()["scale"][1].get<GLfloat>(), it.value()["scale"][2].get<GLfloat>());

		model->SetIdentifier(it.key());

		if (it.value()["shader"]["reflections"]["fallback"]["reflection"].is_array())
		{
			num_reflections = (int)it.value()["shader"]["reflections"]["fallback"]["reflection"].size();
		}
		else
		{
			num_reflections = 1;
		}

		//load shader
		shader_description.shaders = GetShaders(path, config, it.value()["shader"]["render"]);
		shader_description.preprocessor_defines = {
			{"POINT_LIGHT_NUM", std::to_string(num_point_lights)},
			{"DATA_TEX_NUM", std::to_string(ENGINECANVAS_NUM_DATA_TEX)},
			{"APPROXIMATION_OBB_NUM", std::to_string(approximation->NumOBBs())},
			{"REFLECTION_NUM", std::to_string(num_reflections)}
		};
		shader_program = scene->GetShaderProgram(shader_description);
		
		Material mat;
		mat.SetDiffuse(glm::vec3(it.value()["shader"]["phong"]["diffuse"][0].get<float>(),
			it.value()["shader"]["phong"]["diffuse"][1].get<float>(),
			it.value()["shader"]["phong"]["diffuse"][2].get<float>()));
		mat.SetSpecular(glm::vec3(it.value()["shader"]["phong"]["specular"][0].get<float>(),
			it.value()["shader"]["phong"]["specular"][1].get<float>(),
			it.value()["shader"]["phong"]["specular"][2].get<float>()));
		mat.SetSpecularHighlight(it.value()["shader"]["phong"]["specular highlight"].get<float>());

		if (it.value()["shader"]["reflections"]["screen space"].is_boolean())
		{
			mat.SetSSRConfig(default_ssr);
			mat.EnableSSR(it.value()["shader"]["reflections"]["screen space"].get<bool>());
		}
		else
		{
			MaterialSSRConfig ssr_config = default_ssr;
			auto config = it.value()["shader"]["reflections"]["screen space"];

			if (config["resolution"].is_number())
			{
				ssr_config.resolution = config["resolution"].get<float>();
			}
			
			if (config["distance limit"].is_number_float())
			{
				ssr_config.cast_distance_limit = config["distance limit"].get<float>();
			}
			
			if (config["acceptable depth distance"].is_number_float())
			{
				ssr_config.depth_acceptance = config["acceptable depth distance"].get<float>();
			}
			
			if (config["max camera distance"].is_number_float())
			{
				ssr_config.max_cam_distance = config["max camera distance"].get<float>();
			}

			if (config["appear in ssr"].is_boolean())
			{
				ssr_config.appear_in_ssr = config["appear in ssr"].get<bool>();
			}

			if (config["refinements"].is_number_integer())
			{
				ssr_config.refinements = config["refinements"].get<int>();
			}

			mat.SetSSRConfig(ssr_config);

			mat.EnableSSR(it.value()["shader"]["reflections"]["screen space"]["enabled"].get<bool>());
		}

		if (it.value()["shader"]["reflections"]["fallback"]["mode"].is_string())
		{
			if (it.value()["shader"]["reflections"]["fallback"]["mode"].get<std::string>() == "iterative sampling")
			{
				reflection_mode = 0;
			}
			else if (it.value()["shader"]["reflections"]["fallback"]["mode"].get<std::string>() == "oriented bounding box")
			{
				reflection_mode = 1;
			}
			else
			{
				throw std::runtime_error("Unknown reflection mode " + it.value()["shader"]["reflections"]["fallback"]["mode"].get<std::string>());
			}
		}
		else
		{
			reflection_mode = it.value()["shader"]["reflections"]["fallback"]["mode"].get<int>();
		}

		if (it.value()["shader"]["reflections"]["fallback"]["reflection"].is_array())
		{
			int i = 0;
			for (auto it2 = it.value()["shader"]["reflections"]["fallback"]["reflection"].begin(); it2 != it.value()["shader"]["reflections"]["fallback"]["reflection"].end(); it2++)
			{
				mat.AddReflection((Reflection*)scene->GetByIdentifier(it2.value().get<std::string>(), 3), reflection_mode);
			}
		}
		else
		{
			mat.AddReflection((Reflection*)scene->GetByIdentifier(it.value()["shader"]["reflections"]["fallback"]["reflection"].get<std::string>(), 3), reflection_mode);
		}

		model->SetMaterial(mat);

		// load texture
		CreateTexture(shader_program, "colourTexture", path, it.value()["shader"]["textures"]["colour"], 1.0f, 1.0f, 1.0f);
		CreateTexture(shader_program, "normalTexture", path, it.value()["shader"]["textures"]["normal"], 0.5f, 0.5f, 1.0f);
		CreateTexture(shader_program, "specularTexture", path, it.value()["shader"]["textures"]["specular"], 0.0f, 0.0f, 0.0f);
		CreateTexture(shader_program, "reflectionIntensityTexture", path, it.value()["shader"]["textures"]["reflection intensity"], 0.0f, 0.0f, 0.0f);
		CreateTexture(shader_program, "skyboxMaskTexture", path, it.value()["shader"]["textures"]["skybox mask"], 0.0f, 0.0f, 0.0f);

		// store shader program
		model->SetShaderProgram(shader_program);

		//load shadow shader
		shader_program = new ShaderProgram(GetShaders(path, config, it.value()["shader"]["shadow"]), {});
		model->SetShadowShaderProgram(shader_program);

		//flip normals if required
		if (it.value()["invert normals"].is_boolean() && it.value()["invert normals"].get<bool>())
		{
			model->InvertNormals();
		}

		//add to correct vis box
		if (it.value()["vis box"].is_string())
		{
			visboxes.at(it.value()["vis box"].get<std::string>())->AddMemberModel(model);
		}
		else if (it.value()["vis box"].is_array())
		{
			for (auto it2 = it.value()["vis box"].begin(); it2 != it.value()["vis box"].end(); it2++)
			{
				visboxes.at(it2.value().get<std::string>())->AddMemberModel(model);
			}
		}
		else
		{
			for (int i = 0; i < (int)scene->visboxes.size(); i++)
			{
				scene->visboxes.at(i)->AddMemberModel(model);
			}
		}

		//store model
		scene->AddModel(model);
	}

	// delete model lib
	for (std::map<std::string, Model*>::iterator it = model_lib.begin(); it != model_lib.end(); it++)
	{
		delete it->second;
	}

	//make skybox scene (if specified)
	if (config["metadata"]["skybox scene"].is_object())
	{
		Scene* skybox_scene = InitialiseScene(path, config["metadata"]["skybox scene"]["file"].get<std::string>());
		scene->SetSkyboxScene(skybox_scene);
		scene->InitialiseSkyboxTexture(config["metadata"]["skybox scene"]["texture"][0].get<unsigned int>(),
			config["metadata"]["skybox scene"]["texture"][1].get<unsigned int>());
	}

	if (config["metadata"]["background colour"].is_array())
	{
		scene->SetClearColour(glm::vec4(config["metadata"]["background colour"][0].get<float>(),
			config["metadata"]["background colour"][1].get<float>(),
			config["metadata"]["background colour"][2].get<float>(),
			config["metadata"]["background colour"][3].get<float>()));
	}

	return scene;
}

std::vector<std::tuple<std::string, GLenum>> GetShaders(std::string base_path, nlohmann::json config, nlohmann::basic_json<> shader_config)
{
	std::vector<std::tuple<std::string, GLenum>> output = {};

	int shader_type;
	int j = (int)shader_config.size();
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

void CreateTexture(ShaderProgram* shader_program, std::string shader_name, std::string base_path, json image_specifier, float default_r, float default_g, float default_b)
{
	//load image
	wxImage image;

	float r;
	float g;
	float b;
	bool generate_texture = false;

	GLuint mag_filter = GL_NEAREST;
	GLuint min_filter = GL_NEAREST;

	if (image_specifier.is_string()) //image is given as a file path
	{
		image.LoadFile(base_path + '/' + image_specifier.get<std::string>(), wxBITMAP_TYPE_ANY);

		mag_filter = GL_LINEAR;
		min_filter = GL_LINEAR;
	}
	else if (image_specifier.is_object())
	{
		image.LoadFile(base_path + '/' + image_specifier["path"].get<std::string>(), wxBITMAP_TYPE_ANY);

		if (image_specifier.contains("magnify filter"))
		{
			if (image_specifier["magnify filter"].get<std::string>() == "linear")
			{
				mag_filter = GL_LINEAR;
			}
			else if (image_specifier["magnify filter"].get<std::string>() == "nearest")
			{
				mag_filter = GL_NEAREST;
			}
			else
			{
				throw std::runtime_error("Invalid texture filter " + image_specifier["magnify filter"].get<std::string>());
			}
		}

		if (image_specifier.contains("shrink filter"))
		{
			if (image_specifier["shrink filter"].get<std::string>() == "linear")
			{
				min_filter = GL_LINEAR;
			}
			else if (image_specifier["shrink filter"].get<std::string>() == "nearest")
			{
				min_filter = GL_NEAREST;
			}
			else
			{
				throw std::runtime_error("Invalid texture filter " + image_specifier["shrink filter"].get<std::string>());
			}
		}
	}
	else if (image_specifier.is_array() && (image_specifier.size() == 3))
	{
		r = image_specifier[0].get<float>();
		g = image_specifier[1].get<float>();
		b = image_specifier[2].get<float>();

		generate_texture = true;
	}
	else
	{
		r = default_r;
		g = default_g;
		b = default_b;

		generate_texture = true;
	}

	if (generate_texture)
	{
		unsigned char* data = (unsigned char*)malloc(3 * sizeof(unsigned char));
		data[0] = (unsigned char)(r * ((2 << 7) - 1));
		data[1] = (unsigned char)(g * ((2 << 7) - 1));
		data[2] = (unsigned char)(b * ((2 << 7) - 1));

		image.SetData(data, 1, 1);

		mag_filter = GL_NEAREST;
		min_filter = GL_NEAREST;
	}

	if (!image.IsOk())
	{
		throw std::runtime_error("Error while creating image bound to " + shader_name);
	}

	//send image to GPU
	shader_program->LoadTexture(shader_name, image.GetData(), image.GetWidth(), image.GetHeight(), -1, min_filter, mag_filter);
}