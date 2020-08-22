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

		file_stream.open(root_path / file_name);
		if (file_stream.is_open())
		{
			while (std::getline(file_stream, line_contents))
			{
				file_contents = file_contents + line_contents + '\n';
			}
		}
		else
		{
			throw std::invalid_argument("Can't open file at " + (root_path / file_name).string());
		}

		scene_data = nlohmann::json::parse(file_contents);
	}

	//load all models
	std::map<std::string, ModelGeometry> geometry;
	if (scene_data["models"]["ply"].is_object())
	{
		for (auto it = scene_data["models"]["ply"].begin(); it != scene_data["models"]["ply"].end(); it++)
		{
			ModelGeometry model_geometry = ModelFromPly((root_path / it.key()).string());

			if (it.value()["merge geometry"].is_object())
			{
				if (it.value()["merge geometry"]["enable"].is_boolean() && it.value()["merge geometry"]["distance"].is_number())
				{
					if (it.value()["merge geometry"]["enable"].get<bool>())
					{
						double merge_distance = it.value()["merge geometry"]["distance"].get<double>();
						if (merge_distance >= 0)
						{
							MergeVertices(model_geometry, merge_distance);
						}
						else
						{
							throw std::runtime_error("Merge distance is less than 0 (" + std::to_string(merge_distance) + ")");
						}
					}
				}
				else
				{
					throw std::runtime_error("Merge geometry field must contain 'enabled' (bool) and 'distance' (number)");
				}
			}

			if (it.value()["invert normals"].is_boolean())
			{
				if (it.value()["invert normals"].get<bool>())
				{
					InvertNormals(model_geometry);
				}
			}

			std::string model_name;
			if (it.value()["identifier"].is_string())
			{
				model_name = it.value()["identifier"].get<std::string>();
			}
			else
			{
				throw std::runtime_error("A string in field 'identifier' must be provided for each model load");
			}

			if (geometry.count(model_name) == 0)
			{
				geometry.insert(std::pair(model_name, model_geometry));
			}
			else
			{
				throw std::runtime_error("Model identifier '" + model_name + "' is not unique");
			}
		}
	}

	//initialise scene object
	Scene* scene = new Scene();
	scene->ManageChildren(true);

	if (scene_data["metadata"].is_object())
	{
		if (scene_data["metadata"]["identifier"].is_string())
		{
			scene->SetIdentifier(scene_data["metadata"]["identifier"].get<std::string>());
		}
	}

	scene->SetAmbientLight(GetVector(scene_data["lighting"]["ambient"], glm::dvec3(0.0)));

	//load scene approximation (OBBs)
	if (scene_data["obb approximation"].is_array())
	{
		for (auto& el : scene_data["obb approximation"].items())
		{
			OrientedBoundingBox obb;

			if (el.value()["identifier"].is_string())
			{
				obb.SetIdentifier(el.value()["identifier"].get<std::string>());
			}

			obb.SetPosition(GetVector(el.value()["position"], glm::dvec3(0.0)));
			obb.SetRotation(GetVector(el.value()["rotation"], glm::dvec3(0.0)));
			obb.SetScale(GetVector(el.value()["rotation"], glm::dvec3(2.0)) * 0.5);

			scene->Add(obb);
		}
	}

	//load potentially visible sets
	if (scene_data["visboxes"].is_object())
	{

		//first pass - load visboxes
		for (auto& el : scene_data["visboxes"].items())
		{
			VisBox* visbox = new VisBox();
			
			visbox->SetIdentifier(el.key());
			visbox->SetPosition(GetVector(el.value()["position"], glm::dvec3(0.0)));
			visbox->SetRotation(GetVector(el.value()["rotation"], glm::dvec3(0.0)));
			visbox->SetScale(GetVector(el.value()["dimensions"], glm::dvec3(2.0)) * 0.5);

			scene->Add(visbox);
		}

		//second pass - load visboxes into other visboxes
		for (auto& el : scene_data["visboxes"].items())
		{
			VisBox* visbox = scene->GetVisBox(el.key());
			
			std::vector<std::string> pvs;
			if (el.value()["pvs"].is_string())
			{
				pvs.push_back(el.value()["pvs"].get<std::string>());
			}
			else if (el.value()["pvs"].is_array())
			{
				for (auto& el2 : el.value()["pvs"].items())
				{
					if (el2.value().is_string())
					{
						pvs.push_back(el2.value().get<std::string>());
					}
					else
					{
						throw std::runtime_error("Visbox potentially visible set must be specified as a string or an array of strings");
					}
				}
			}
			else
			{
				throw std::runtime_error("Visbox potentially visible set must be specified as a string or an array of strings");
			}

			for (auto& el2 : pvs)
			{
				VisBox* inner_visbox = scene->GetVisBox(el2);
				if (inner_visbox == nullptr)
				{
					throw std::runtime_error("Unknown visbox identifier '" + el2 + "' in PVS");
				}
				else
				{
					visbox->AddPotentiallyVisible(inner_visbox);
				}
			}
		}
	}

	//load models without reflections in materials
	std::vector<Model*> models; //models stored in the order that they appear in in the scene file
	{
		//function to apply an ssr config specified in json to a MaterialSSRConfig struct
		auto ApplySSRConfig = [](MaterialSSRConfig config, nlohmann::json data)
		{
			if (data.is_object())
			{
				if (data.is_number())
				{
					config.depth_acceptance = data["acceptable depth distance"].get<float>();
				}

				if (data["appear in ssr"].is_boolean())
				{
					config.appear_in_ssr = data["appear in ssr"].get<bool>();
				}

				if (data["distance limit"].is_number())
				{
					config.cast_distance_limit = data["distance limit"].get<float>();
				}

				if (data["max camera distance"].is_number())
				{
					config.max_cam_distance = data["max camera distance"].get<float>();
				}

				if (data["refinements"].is_number_integer())
				{
					config.refinements = data["refinements"].get<int>();
				}

				if (data["resolution"].is_number())
				{
					config.resolution = data["resolution"].get<float>();
				}
			}

			return config;
		};

		MaterialSSRConfig default_ssr_config = ApplySSRConfig(MaterialSSRConfig(), scene_data["ssr defaults"]);;

		//load all models with correct materials
		if (scene_data["layout"].is_array())
		{
			for (auto& el : scene_data["layout"].items())
			{
				Model* model;
				if (el.value()["model"].is_string())
				{
					auto model_geometry = geometry.find(el.value()["model"].get<std::string>());
					if (model_geometry == geometry.end())
					{
						throw std::runtime_error("Model '" + el.value()["model"].get<std::string>() + "' has not been loaded");
					}
					else
					{
						model = new Model(scene->GetNewModelReference(), model_geometry->second, scene);

						if (el.value()["identifier"].is_string())
						{
							model->SetIdentifier(el.value()["identifier"].get<std::string>());
						}

						//load ssr reflections
						model->GetMaterial().ssr = default_ssr_config;

						if (el.value()["reflections"].is_object())
						{
							if (el.value()["reflections"]["screen space"].is_object())
							{
								MaterialSSRConfig ssr_config = ApplySSRConfig(default_ssr_config, el.value()["reflections"]["screen space"]);

								model->GetMaterial().ssr = ssr_config;

								if (el.value()["reflections"]["screen space"]["enabled"].is_boolean())
								{
									model->GetMaterial().ssr_enabled = el.value()["reflections"]["screen space"]["enabled"].get<bool>();
								}
							}
							else if (el.value()["reflections"]["screen space"].is_boolean())
							{
								model->GetMaterial().ssr_enabled = el.value()["reflections"]["screen space"].get<bool>();
							}

							//don't set up the alternative fallback reflections as they will be set up once the reflections have been created
						}

						//load textures
						model->GetColourTexture() = GetTexture(el.value()["textures"]["colour"], root_path, scene->GetNewTextureReference(), glm::vec3(1.0f));
						model->GetNormalTexture() = GetTexture(el.value()["textures"]["normal"], root_path, scene->GetNewTextureReference(), glm::vec3(0.5f, 0.5f, 1.0f));
						model->GetReflectionTexture() = GetTexture(el.value()["textures"]["reflection intensity"], root_path, scene->GetNewTextureReference(), glm::vec3(0.0f));
						model->GetSpecularTexture() = GetTexture(el.value()["textures"]["specular"], root_path, scene->GetNewTextureReference(), glm::vec3(0.0f));
						model->GetSkyboxMaskTexture() = GetTexture(el.value()["textures"]["skybox mask"], root_path, scene->GetNewTextureReference(), glm::vec3(0.0f));

						//load phong material
						model->GetMaterial().diffuse = GetVector(el.value()["material"]["diffuse"], glm::dvec3(0.0));
						model->GetMaterial().specular = GetVector(el.value()["material"]["specular"], glm::dvec3(0.0));

						if (el.value()["material"]["specular highlight"].is_number())
						{
							model->GetMaterial().specular_highlight = el.value()["material"]["specular highlight"].get<float>();
						}
					}

					//add model to potentially visible sets
					std::vector<std::string> pvs_membership;
					if (el.value()["visboxes"].is_array())
					{
						for (auto& el2 : el.value()["visboxes"].items())
						{
							if (el2.value().is_string())
							{
								pvs_membership.push_back(el2.value().get<std::string>());
							}
							else
							{
								throw std::runtime_error("Model PVS membership must be specified by either a string or an array of strings");
							}
						}
					}
					else if (el.value()["visboxes"].is_string())
					{
						pvs_membership.push_back(el.value()["visboxes"].get<std::string>());
					}
					else
					{
						throw std::runtime_error("Model PVS membership must be specified by either a string or an array of strings");
					}

					for (auto& el2 : pvs_membership)
					{
						VisBox* visbox = scene->GetVisBox(el2);

						if (visbox == nullptr)
						{
							throw std::runtime_error("Unknown PVS identifier '" + el2 + "' used when specifying model PVS membership");
						}
						else
						{
							visbox->AddMemberModel(model);
						}
					}

					scene->Add(model);
					models.push_back(model);
				}
				else
				{
					throw std::runtime_error("No model specified");
				}
			}
		}
	}

	//load all reflections with model ptrs
	for (auto& el : scene_data["reflections"].items())
	{
		Reflection* reflection = new Reflection(scene->GetNewRenderTextureReference());

		reflection->SetPosition(GetVector(el.value()["position"], glm::dvec3(0.0)));

		if (el.value()["identifier"].is_string())
		{
			reflection->SetIdentifier(el.value()["identifier"].get<std::string>());
		}

		if (el.value()["corrections"].is_object())
		{
			if (el.value()["corrections"]["iterative sampling"].is_object())
			{
				if (el.value()["corrections"]["iterative sampling"]["iterations"].is_number_integer())
				{
					reflection->SetIterations(el.value()["corrections"]["iterative sampling"]["iterations"].get<int>());
				}
			}
		}

		ConfigureCubemap(el.value(), reflection, scene);

		scene->Add(reflection);
	}

	//load reflection ptrs into all models that require them for reflections
	if (scene_data["layout"].is_array())
	{
		for (int i = 0; i < (int)scene_data["layout"].size(); i++)
		{
			auto& el = scene_data["layout"][i];
			if (el["reflections"]["alternative"].is_object())
			{
				ReflectionMode mode;
				if (el["reflections"]["alternative"]["mode"].is_string())
				{
					std::string mode_name = el["reflections"]["alternative"]["mode"].get<std::string>();
					if (mode_name == "iterative sampling")
					{
						mode = ReflectionMode::Iterative;
					}
					else if (mode_name == "oriented bounding box")
					{
						mode = ReflectionMode::OBB;
					}
					else
					{
						throw std::runtime_error("Unknown alternative reflection mode '" + mode_name + "'");
					}
				}
				else
				{
					throw std::runtime_error("An alternative reflection mode string must be provided");
				}

				std::vector<std::string> refl_names;
				if (el["reflections"]["alternative"]["reflection"].is_string())
				{
					refl_names.push_back(el["reflections"]["alternative"]["reflection"].get<std::string>());
				}
				else if (el["reflections"]["alternative"]["reflection"].is_array())
				{
					for (auto& el2 : (el["reflections"]["alternative"]["reflection"].items()))
					{
						if (el2.value().is_string())
						{
							refl_names.push_back(el2.value().get<std::string>());
						}
						else
						{
							throw std::runtime_error("Each reflection identifier specified in the alternative reflection mode for a model must be a string");
						}
					}
				}
				else
				{
					throw std::runtime_error("Alternative reflections must either be specified as a string or an array of strings");
				}

				for (auto& el2 : refl_names)
				{
					Reflection* reflection = scene->GetReflection(el2);
					if (reflection == nullptr)
					{
						throw std::runtime_error("Couldn't resolve alternative reflection for model with identifier '" + el2 + "'");
					}
					else
					{
						Model* model = models.at(i);
						model->GetMaterial().reflections.push_back(std::tuple(reflection, mode));
					}
				}
			}
		}
	}

	//load all point lights
	if (scene_data["lighting"]["point lights"].is_array())
	{
		for (auto& el : scene_data["lighting"]["point lights"].items())
		{
			if (el.value().is_object())
			{
				PointLight* pointlight = new PointLight(scene->GetNewRenderTextureReference());

				pointlight->SetPosition(GetVector(el.value()["position"], glm::dvec3(0.0)));
				pointlight->SetIntensity(GetVector(el.value()["intensity"], glm::dvec3(0.0)));

				if (el.value()["shadows"].is_object())
				{
					if (el.value()["shadows"]["enabled"].is_boolean())
					{
						pointlight->SetShadowBias(el.value()["shadows"]["enabled"].get<bool>());
					}

					if (el.value()["shadows"]["acceptance bias"].is_number())
					{
						pointlight->SetShadowBias(el.value()["shadows"]["acceptance bias"].get<double>());
					}

					ConfigureCubemap(el.value()["shadows"], pointlight, scene);
				}

				scene->Add(pointlight);
			}
			else
			{
				throw std::runtime_error("All point lights must be specified as objects");
			}
		}
	}

	return scene;
}

Scene* SceneFromJSON(std::string root_path, std::string file_name)
{
	return SceneFromJSON(std::filesystem::path(root_path), std::filesystem::path(file_name));
}

LocalTexture GetTexture(nlohmann::json data, std::filesystem::path root_path, TextureReference reference, glm::vec3 default_value)
{
	LocalTexture texture(reference);

	if (data.is_string())
	{
		wxImage image;
		std::filesystem::path img_path = root_path / data.get<std::string>();
		image.LoadFile(img_path.string());

		if (!image.IsOk())
		{
			throw std::runtime_error("Error while loading image at '" + img_path.string() + "'");
		}

		texture.SetFullTexture(image.GetData(), { image.GetWidth(), image.GetHeight() }, true);
	}
	else if (data.is_object())
	{
		if (data["texture"].is_string())
		{
			texture = GetTexture(data["texture"], root_path, reference, default_value);
			
			if (data["magnify filter"].is_string())
			{
				std::string filter = data["magnify filter"].get<std::string>();
				if (filter == "nearest")
				{
					texture.SetMagFilter(LocalTextureFilter::Nearest);
				}
				else if (filter == "linear")
				{
					texture.SetMagFilter(LocalTextureFilter::Linear);
				}
				else
				{
					throw std::runtime_error("Magnify filter must be either 'linear' or 'nearest', not '" + filter + "'");
				}
			}

			if (data["shrink filter"].is_string())
			{
				std::string filter = data["shrink filter"].get<std::string>();
				if (filter == "nearest")
				{
					texture.SetMinFilter(LocalTextureFilter::Nearest);
				}
				else if (filter == "linear")
				{
					texture.SetMinFilter(LocalTextureFilter::Linear);
				}
				else
				{
					throw std::runtime_error("Shrink filter must be either 'linear' or 'nearest', not '" + filter + "'");
				}
			}
		}
		else
		{
			throw std::runtime_error("No texture specified");
		}
		
	}
	else
	{
		texture.SetVector(GetVector(data, glm::dvec3(default_value)));
	}

	return texture;
}

void ConfigureCubemap(nlohmann::json& data, Cubemap* cubemap, Scene* scene)
{
	if (data["clips"].is_array())
	{
		if (data["clips"].size() == 2)
		{
			cubemap->SetClips({ data["clips"][0].get<double>(), data["clips"][1].get<double>() });
		}
		else
		{
			throw std::runtime_error("Clips for cubemaps must contain exactly 2 values");
		}
	}

	if (data["texture"].is_array())
	{
		if (data["texture"].size() == 2)
		{
			cubemap->SetTextureDimensions({ data["texture"][0].get<int>(), data["texture"][1].get<int>() });
		}
		else
		{
			throw std::runtime_error("Texture dimensions for cubemaps must contain exactly 2 values");
		}
	}

	if (data["static draw"].is_array())
	{
		for (auto& el2 : data["static draw"].items())
		{
			if (el2.value().is_string())
			{
				Model* model = scene->GetModel(el2.value().get<std::string>());
				if (model == nullptr)
				{
					throw std::runtime_error("Cubemap static draw target '" + el2.value().get<std::string>() + "' does not exist");
				}
				else
				{
					cubemap->AddStaticModel(model->GetReference());
				}
			}
			else
			{
				throw std::runtime_error("Cubemap static draw targets must be provided as string identifiers");
			}
		}
	}

	if (data["dynamic draw"].is_array())
	{
		for (auto& el2 : data["dynamic draw"].items())
		{
			if (el2.value().is_string())
			{
				Model* model = scene->GetModel(el2.value().get<std::string>());
				if (model == nullptr)
				{
					throw std::runtime_error("Cubemap dynamic draw target '" + el2.value().get<std::string>() + "' does not exist");
				}
				else
				{
					cubemap->AddDynamicModel(model->GetReference());
				}
			}
			else
			{
				throw std::runtime_error("Cubemap dynamic draw targets must be provided as string identifiers");
			}
		}
	}

	if (data["dynamic draw refresh frames"].is_number())
	{
		if (data["dynamic draw refresh frames"].is_number_integer())
		{
			cubemap->SetFramesRequiredForDynamicRender(data["dynamic draw refresh frames"].get<int>());
		}
		else
		{
			throw std::runtime_error("Cubemap dynamic redraw frames must be a positive integer");
		}
	}
}

template<unsigned int dimensions>
dvec<dimensions> GetVector(nlohmann::json data, dvec<dimensions> default_value)
{
	static_assert(dimensions > 0, "Dimensions must be greater than zero");
	static_assert(dimensions < 5, "Dimensions must be 4 or below");

	if (data.is_array())
	{
		if (data.size() == dimensions)
		{
			bool is_nums = true;
			for (unsigned int i = 0; i < dimensions; i++)
			{
				if (!data.at(i).is_number())
				{
					is_nums = false;
				}
			}

			if (is_nums)
			{
				std::vector<double> values;
				for (auto& el : data.items())
				{
					values.push_back(el.value().get<double>());
				}

				dvec<dimensions> result = default_value;
				for (int i = 0; i < (int)std::min(values.size(), dimensions); i++)
				{
					result[i] = values.at(i);
				}

				return result;
			}
			else
			{
				throw std::runtime_error("All values in a vector must be numbers");
			}
		}
		else
		{
			throw std::runtime_error("Dimensions (" + std::to_string(dimensions) + ") are not equal to the number of given values (" + std::to_string(data.size()) + ")");
		}
	}
	else if (data.is_number())
	{
		return dvec<dimensions>(data.get<double>());
	}
	else
	{
		return default_value;
	}
}

template dvec<1> GetVector<1>(nlohmann::json, dvec<1>);
template dvec<2> GetVector<2>(nlohmann::json, dvec<2>);
template dvec<3> GetVector<3>(nlohmann::json, dvec<3>);
template dvec<4> GetVector<4>(nlohmann::json, dvec<4>);