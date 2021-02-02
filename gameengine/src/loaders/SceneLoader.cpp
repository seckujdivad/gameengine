#include "SceneLoader.h"

#include <stdexcept>
#include <map>
#include <vector>
#include <string>

#include <wx/image.h>

#include <nlohmann/json.hpp>

#include "../scene/LocalTexture.h"
#include "../scene/Referenceable.h"
#include "../scene/Scene.h"
#include "../scene/Cubemap.h"
#include "../scene/VisBox.h"
#include "../scene/Skybox.h"
#include "../scene/light/PointLight.h"
#include "../scene/model/Model.h"
#include "../generic/LoadFile.h"

#include "models/PlyLoader.h"
#include "models/BptLoader.h"

template<unsigned int dimensions>
using dvec = glm::vec<dimensions, glm::f64, glm::packed_highp>;

template<unsigned int dimensions>
dvec<dimensions> GetVector(const nlohmann::json& data, dvec<dimensions> default_value)
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
				for (int i = 0; i < (int)std::min(static_cast<unsigned int>(values.size()), dimensions); i++)
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

LocalTexture GetTexture(const nlohmann::json& data, std::filesystem::path root_path, TextureReference reference, glm::vec3 default_value)
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

		texture.SetFullTexture(image.GetData(), { image.GetWidth(), image.GetHeight() });
	}
	else if (data.is_object())
	{
		if (data.contains("texture") && data["texture"].is_string())
		{
			texture = GetTexture(data["texture"], root_path, reference, default_value);

			if (data.contains("magnify filter") && data["magnify filter"].is_string())
			{
				std::string filter = data["magnify filter"].get<std::string>();
				if (filter == "nearest")
				{
					texture.SetMagFilter(LocalTexture::Filter::Nearest);
				}
				else if (filter == "linear")
				{
					texture.SetMagFilter(LocalTexture::Filter::Linear);
				}
				else
				{
					throw std::runtime_error("Magnify filter must be either 'linear' or 'nearest', not '" + filter + "'");
				}
			}

			if (data.contains("shrink filter") && data["shrink filter"].is_string())
			{
				std::string filter = data["shrink filter"].get<std::string>();
				if (filter == "nearest")
				{
					texture.SetMinFilter(LocalTexture::Filter::Nearest);
				}
				else if (filter == "linear")
				{
					texture.SetMinFilter(LocalTexture::Filter::Linear);
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

void ConfigureCubemap(const nlohmann::json& data, const nlohmann::json& perf_data, Cubemap* cubemap, Scene* scene)
{
	if (data.contains("clips") && data["clips"].is_array())
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

	if (data.contains("texture") && data["texture"].is_number_integer())
	{
		const nlohmann::json& texture_dimensions = perf_data["scene"]["cubemap"]["texture"][data["texture"].get<int>()];
		if (texture_dimensions.is_array())
		{
			if (texture_dimensions.size() == 2)
			{
				cubemap->SetTextureDimensions({ texture_dimensions[0].get<int>(), texture_dimensions[1].get<int>() });
			}
			else
			{
				throw std::runtime_error("Texture dimensions for cubemaps must contain exactly 2 values");
			}
		}
	}
	else
	{
		throw std::runtime_error("An index must be provided for the texture dimensions");
	}

	if (data.contains("static draw") && data["static draw"].is_array())
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

	if (data.contains("dynamic draw") && data["dynamic draw"].is_array())
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

	if (data.contains("dynamic draw refresh frames") && data["dynamic draw refresh frames"].is_number())
	{
		if (data["dynamic draw refresh frames"].is_number_integer())
		{
			cubemap->SetDynamicRedrawFrames(data["dynamic draw refresh frames"].get<int>());
		}
		else
		{
			throw std::runtime_error("Cubemap dynamic redraw frames must be a positive integer");
		}
	}
}


Scene* SceneFromJSON(SceneLoaderConfig config)
{
	//load scene json
	nlohmann::json scene_data = nlohmann::json::parse(LoadFile(config.path.root / config.path.file));

	//load performance settings json
	nlohmann::json perf_data;
	{
		std::filesystem::path perf_profile_path = scene_data["metadata"]["performance profiles"][config.performance.index].get<std::string>();
		perf_data = nlohmann::json::parse(LoadFile(config.path.root / perf_profile_path));
	}

	//load all models
	std::unordered_map<std::string, std::vector<std::shared_ptr<Geometry>>> geometry_lookup;

	if (scene_data["models"]["ply"].is_object())
	{
		for (auto it = scene_data["models"]["ply"].begin(); it != scene_data["models"]["ply"].end(); it++)
		{
			std::shared_ptr<Polygonal> model_geometry = ModelFromPly((config.path.root / it.value()["path"].get<std::string>()).string());

			if (it.value()["merge geometry"].is_object())
			{
				if (it.value()["merge geometry"]["enable"].is_boolean() && it.value()["merge geometry"]["distance"].is_number())
				{
					if (it.value()["merge geometry"]["enable"].get<bool>())
					{
						double merge_distance = it.value()["merge geometry"]["distance"].get<double>();
						if (merge_distance >= 0)
						{
							model_geometry->MergeVertices(merge_distance);
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
					model_geometry->InvertNormals();
				}
			}

			std::string model_name = it.key();

			if (geometry_lookup.count(model_name) == 0)
			{
				geometry_lookup.insert(std::pair(model_name, std::vector<std::shared_ptr<Geometry>>({ std::dynamic_pointer_cast<Geometry>(model_geometry) })));
			}
			else
			{
				throw std::runtime_error("Model identifier '" + model_name + "' is not unique");
			}
		}
	}

	if (scene_data["models"]["bpt"].is_object())
	{
		for (auto it = scene_data["models"]["bpt"].begin(); it != scene_data["models"]["bpt"].end(); it++)
		{
			std::string model_name = it.key();

			std::vector<std::shared_ptr<Patch>> patches = PatchesFromBPT((config.path.root / it.value()["path"].get<std::string>()).string());

			std::vector<std::shared_ptr<Geometry>> geometry;
			geometry.reserve(patches.size());
			for (const std::shared_ptr<Patch>& patch : patches)
			{
				geometry.push_back(std::dynamic_pointer_cast<Geometry>(patch));
			}

			if (geometry_lookup.count(model_name) == 0)
			{
				geometry_lookup.insert(std::pair(model_name, geometry));
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
			obb.SetScale(GetVector(el.value()["dimensions"], glm::dvec3(2.0)) * 0.5);

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
		auto ApplySSRConfig = [&perf_data](MaterialSSRConfig config, const nlohmann::json& data)
		{
			if (data.is_object())
			{
				if (data.contains("acceptable depth distance") && data["acceptable depth distance"].is_number())
				{
					config.depth_acceptance = perf_data["scene"]["model"]["material"]["ssr"]["acceptable depth distance"][data["acceptable depth distance"].get<int>()].get<float>();
				}

				if (data.contains("appear in ssr") && data["appear in ssr"].is_boolean())
				{
					config.appear_in_ssr = data["appear in ssr"].get<bool>();
				}

				if (data.contains("distance limit") && data["distance limit"].is_number())
				{
					config.cast_distance_limit = perf_data["scene"]["model"]["material"]["ssr"]["distance limit"][data["distance limit"].get<int>()].get<float>();
				}

				if (data.contains("max camera distance") && data["max camera distance"].is_number())
				{
					config.max_cam_distance = perf_data["scene"]["model"]["material"]["ssr"]["max camera distance"][data["max camera distance"].get<int>()].get<float>();
				}

				if (data.contains("refinements") && data["refinements"].is_number_integer())
				{
					config.refinements = perf_data["scene"]["model"]["material"]["ssr"]["refinements"][data["refinements"].get<int>()].get<int>();
				}

				if (data.contains("resolution") && data["resolution"].is_number())
				{
					config.resolution = perf_data["scene"]["model"]["material"]["ssr"]["resolution"][data["resolution"].get<int>()].get<float>();
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
				std::vector<std::string> geometry_names;
				
				if (el.value()["model"].is_string())
				{
					geometry_names.push_back(el.value()["model"].get<std::string>());
				}

				if (el.value()["model"].is_array())
				{
					for (auto& el2 : el.value()["model"].items())
					{
						if (el2.value().is_string())
						{
							geometry_names.push_back(el2.value().get<std::string>());
						}
						else
						{
							throw std::runtime_error("All elements in \"model\" must be strings");
						}
					}
				}

				if (el.value()["models"].is_string())
				{
					geometry_names.push_back(el.value()["models"].get<std::string>());
				}

				if (el.value()["models"].is_array())
				{
					for (auto& el2 : el.value()["models"].items())
					{
						if (el2.value().is_string())
						{
							geometry_names.push_back(el2.value().get<std::string>());
						}
						else
						{
							throw std::runtime_error("All elements in \"models\" must be strings");
						}
					}
				}

				std::vector<std::shared_ptr<Geometry>> geometries;
				for (std::string geometry_name : geometry_names)
				{
					if (geometry_lookup.count(geometry_name) == 0)
					{
						throw std::runtime_error("Model geometry '" + geometry_name + "' has not been loaded");
					}
					else
					{
						for (const std::shared_ptr<Geometry>& geometry : geometry_lookup.at(geometry_name))
						{
							geometries.push_back(geometry);
						}
					}
				}

				Model* model = new Model(scene->GetNewModelReference(), geometries, scene);

				if (el.value()["identifier"].is_string())
				{
					model->SetIdentifier(el.value()["identifier"].get<std::string>());
				}

				model->SetPosition(GetVector(el.value()["position"], glm::dvec3(0.0)));
				model->SetRotation(GetVector(el.value()["rotation"], glm::dvec3(0.0)));
				model->SetScale(GetVector(el.value()["scale"], glm::dvec3(1.0)));

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
				else if (el.value()["reflections"].is_boolean())
				{
					if (el.value()["reflections"].get<bool>())
					{
						throw std::runtime_error("If \"reflections\" is a bool, it must be false (i.e. disabling reflections). If you want to enable reflections, you must provide more configuration data");
					}
				}

				//load textures
				LocalTexture colour_texture = GetTexture(el.value()["textures"]["colour"], config.path.root, scene->GetNewTextureReference(), glm::vec3(1.0f));
				LocalTexture normal_texture = GetTexture(el.value()["textures"]["normal"], config.path.root, scene->GetNewTextureReference(), glm::vec3(0.5f, 0.5f, 1.0f));
				LocalTexture refl_texture = GetTexture(el.value()["textures"]["reflection intensity"], config.path.root, scene->GetNewTextureReference(), glm::vec3(0.0f));
				LocalTexture specular_texture = GetTexture(el.value()["textures"]["specular"], config.path.root, scene->GetNewTextureReference(), glm::vec3(0.0f));
				LocalTexture skybox_texture = GetTexture(el.value()["textures"]["skybox mask"], config.path.root, scene->GetNewTextureReference(), glm::vec3(0.0f));
				LocalTexture displacement_texture = GetTexture(el.value()["textures"]["displacement"], config.path.root, scene->GetNewTextureReference(), glm::vec3(0.0f));

				model->GetColourTexture() = colour_texture;;
				model->GetNormalTexture() = normal_texture;
				model->GetReflectionTexture() = refl_texture;
				model->GetSpecularTexture() = specular_texture;
				model->GetSkyboxMaskTexture() = skybox_texture;
				model->GetDisplacementTexture() = displacement_texture;

				//load phong material
				model->GetMaterial().diffuse = GetVector(el.value()["material"]["diffuse"], glm::dvec3(0.0));
				model->GetMaterial().specular = GetVector(el.value()["material"]["specular"], glm::dvec3(0.0));

				if (el.value()["material"]["specular highlight"].is_number())
				{
					model->GetMaterial().specular_highlight = el.value()["material"]["specular highlight"].get<float>();
				}

				if (el.value()["material"]["displacement"].is_object())
				{
					if (el.value()["material"]["displacement"]["multiplier"].is_number())
					{
						model->GetMaterial().displacement.multiplier = el.value()["material"]["displacement"]["multiplier"].get<float>();
					}
					else
					{
						model->GetMaterial().displacement.multiplier = 0.0f;
					}

					if (el.value()["material"]["displacement"]["discard out of range"].is_boolean())
					{
						model->GetMaterial().displacement.discard_out_of_range = el.value()["material"]["displacement"]["discard out of range"].get<bool>();
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

		if (el.value()["draw shadows"].is_boolean())
		{
			reflection->SetDrawShadows(el.value()["draw shadows"].get<bool>()); 
		}

		if (el.value()["draw reflections"].is_boolean())
		{
			reflection->SetDrawReflections(el.value()["draw reflections"].get<bool>());
		}

		ConfigureCubemap(el.value(), perf_data, reflection, scene);

		scene->Add(reflection);
	}

	//load reflection ptrs into all models that require them for reflections
	if (scene_data["layout"].is_array())
	{
		for (int i = 0; i < (int)scene_data["layout"].size(); i++)
		{
			auto& el = scene_data["layout"][i];
			Model* model = models.at(i);

			if (el["reflections"].is_object() && el["reflections"]["alternative"].is_object())
			{
				model->GetMaterial().reflections_enabled = true;

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
						model->GetMaterial().reflections.push_back(std::tuple(reflection, mode));
					}
				}
			}
			else
			{
				model->GetMaterial().reflections_enabled = false;
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
					if (el.value()["shadows"]["acceptance bias"].is_number())
					{
						pointlight->SetShadowBias(el.value()["shadows"]["acceptance bias"].get<double>());
					}

					ConfigureCubemap(el.value()["shadows"], perf_data, pointlight, scene);
				}

				scene->Add(pointlight);
			}
			else
			{
				throw std::runtime_error("All point lights must be specified as objects");
			}
		}
	}

	//load all skyboxes
	{
		for (auto& el : scene_data["skyboxes"].items())
		{
			if (el.value().is_object())
			{
				Skybox* skybox = new Skybox(scene->GetNewRenderTextureReference());
				skybox->SetPosition(GetVector(el.value()["position"], glm::dvec3(0.0)));

				ConfigureCubemap(el.value(), perf_data, skybox, scene);

				scene->Add(skybox);

				for (auto& el2 : el.value()["drawn on"].items())
				{
					if (el2.value().is_string())
					{
						Model* model = scene->GetModel(el2.value().get<std::string>());

						if (model == nullptr)
						{
							throw std::runtime_error("Invalid model identifier \"" + el2.value().get<std::string>() + "\"");
						}
						else
						{
							model->SetSkybox(skybox);
						}
					}
					else
					{
						throw std::runtime_error("All models that have this skybox drawn on must be specified as strings");
					}
				}
			}
			else
			{
				throw std::runtime_error("All skyboxes must be specified as objects");
			}
		}
	}

	return scene;
}
