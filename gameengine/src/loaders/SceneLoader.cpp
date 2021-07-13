#include "SceneLoader.h"

#include <stdexcept>
#include <map>
#include <vector>
#include <string>
#include <thread>

#include <wx/image.h>

#include <nlohmann/json.hpp>

#include "../scene/Referenceable.h"
#include "../scene/Scene.h"
#include "../scene/Cubemap.h"
#include "../scene/VisBox.h"
#include "../scene/Skybox.h"
#include "../scene/texture/Texture.h"
#include "../scene/texture/TextureFiltering.h"
#include "../scene/texture/Generators.h"
#include "../scene/light/PointLight.h"
#include "../scene/model/Model.h"

#include "../generic/LoadFile.h"
#include "../generic/ThreadNamer.h"

#include "models/PlyLoader.h"
#include "models/BptLoader.h"
#include "models/AsyncLoader.h"

#include "GetVector.h"

Texture GetTexture(const nlohmann::json& data, std::filesystem::path root_path, TextureReference reference, glm::vec3 default_value, TextureFiltering default_mag_filter, TextureFiltering default_min_filter)
{
	Texture texture(reference);
	texture.SetMagFilter(default_mag_filter);
	texture.SetMinFilter(default_min_filter);

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
		if (data.contains("texture"))
		{
			if (data["texture"].is_string())
			{
				texture = GetTexture(data["texture"], root_path, reference, default_value, default_mag_filter, default_min_filter);
			}
			else if (data["texture"].is_object() && data["texture"].contains("preset") && data["texture"]["preset"].is_string())
			{
				const std::string preset = data["texture"]["preset"].get<std::string>();
				if (preset == "xor")
				{
					XORType xor_type = XORType::Greyscale;
					if (data["texture"].contains("mode") && data["texture"]["mode"].is_string())
					{
						const std::string xor_type_string = data["texture"]["mode"].get<std::string>();
						if (xor_type_string == "greyscale")
						{
							xor_type = XORType::Greyscale;
						}
						else if (xor_type_string == "hsv")
						{
							xor_type = XORType::HSV;
						}
						else
						{
							throw std::runtime_error("Invalid XOR type \"" + xor_type_string + "\"");
						}
					}

					glm::ivec2 dimensions = GetVector(data["texture"]["dimensions"], glm::ivec2(1, 1));

					GenerateXORTexture(texture, std::tuple<int, int>(dimensions.x, dimensions.y), xor_type);
				}
				else
				{
					throw std::runtime_error("Invalid preset \"" + preset + "\"");
				}
			}
			else
			{
				throw std::runtime_error("Texture must be specified as either an object containing a string member \"preset\" or a string");
			}
		}
		else
		{
			throw std::runtime_error("No texture specified");
		}

		if (data.contains("magnify filter") && data["magnify filter"].is_string())
		{
			std::string filter = data["magnify filter"].get<std::string>();
			if (filter == "nearest")
			{
				texture.SetMagFilter(TextureFiltering::Nearest);
			}
			else if (filter == "linear")
			{
				texture.SetMagFilter(TextureFiltering::Linear);
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
				texture.SetMinFilter(TextureFiltering::Nearest);
			}
			else if (filter == "linear")
			{
				texture.SetMinFilter(TextureFiltering::Linear);
			}
			else
			{
				throw std::runtime_error("Shrink filter must be either 'linear' or 'nearest', not '" + filter + "'");
			}
		}
	}
	else
	{
		texture.SetVector(GetVector(data, glm::dvec3(default_value)));
	}

	return texture;
}

void ConfigureCubemap(const nlohmann::json& data, const nlohmann::json& perf_data, Cubemap* cubemap, std::shared_ptr<Scene> scene)
{
	glm::dvec2 clips = GetVector(data["clips"], glm::dvec2(std::get<0>(cubemap->GetClips()), std::get<1>(cubemap->GetClips())));
	cubemap->SetClips({ clips.x, clips.y });

	if (data.contains("texture") && data["texture"].is_number_integer())
	{
		glm::ivec2 tex_dimensions = GetVector(perf_data["scene"]["cubemap"]["texture"][data["texture"].get<int>()], glm::ivec2(1));
		cubemap->SetTextureDimensions({ tex_dimensions.x, tex_dimensions.y });
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
				std::optional<std::shared_ptr<Model>> model = scene->GetModel(el2.value().get<std::string>());
				if (model.has_value())
				{
					cubemap->AddStaticModel(model.value()->GetReference());
				}
				else
				{
					throw std::runtime_error("Cubemap static draw target '" + el2.value().get<std::string>() + "' does not exist");
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
				std::optional<std::shared_ptr<Model>> model = scene->GetModel(el2.value().get<std::string>());
				if (model.has_value())
				{
					cubemap->AddDynamicModel(model.value()->GetReference());
				}
				else
				{
					throw std::runtime_error("Cubemap dynamic draw target '" + el2.value().get<std::string>() + "' does not exist");
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
		int dynamic_redraw_frames = data["dynamic draw refresh frames"].get<int>();
		if (dynamic_redraw_frames > 0)
		{
			cubemap->SetDynamicRedrawFrames(dynamic_redraw_frames);
		}
		else
		{
			throw std::runtime_error("Cubemap dynamic redraw frames must be a positive integer");
		}
	}
}

std::tuple<std::shared_ptr<Scene>, std::thread> SceneFromJSON(SceneLoaderConfig config)
{
	//load scene json
	nlohmann::json scene_data = nlohmann::json::parse(LoadFile(config.path.root / config.path.file));

	//load performance settings json
	nlohmann::json perf_data;
	{
		std::filesystem::path perf_profile_path = scene_data["metadata"]["performance profiles"][config.performance.index].get<std::string>();
		perf_data = nlohmann::json::parse(LoadFile(config.path.root / perf_profile_path));
	}

	//initialise scene object
	std::shared_ptr<Scene> scene = std::make_shared<Scene>();

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
			std::shared_ptr<VisBox> visbox = std::make_shared<VisBox>();
			
			visbox->SetIdentifier(el.key());
			visbox->SetPosition(GetVector(el.value()["position"], glm::dvec3(0.0)));
			visbox->SetRotation(GetVector(el.value()["rotation"], glm::dvec3(0.0)));
			visbox->SetScale(GetVector(el.value()["dimensions"], glm::dvec3(2.0)) * 0.5);

			scene->Add(visbox);
		}

		//second pass - load visboxes into other visboxes
		for (auto& el : scene_data["visboxes"].items())
		{
			std::optional<std::shared_ptr<VisBox>> visbox = scene->GetVisBox(el.key());

			if (visbox.has_value())
			{
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
					std::optional<std::shared_ptr<VisBox>> inner_visbox = scene->GetVisBox(el2);
					if (inner_visbox.has_value())
					{
						visbox.value()->AddPotentiallyVisible(inner_visbox.value().get());
					}
					else
					{
						throw std::runtime_error("Unknown visbox identifier '" + el2 + "' in PVS");
					}
				}
			}
			else
			{
				throw std::runtime_error("VisBox '" + el.key() + " does not exist");
			}
		}
	}

	//load models without reflections in materials
	std::vector<std::shared_ptr<Model>> models; //models stored in the order that they appear in in the scene file
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

				if (data.contains("min refinements") && data["min refinements"].is_number_integer())
				{
					config.refinements_min = perf_data["scene"]["model"]["material"]["ssr"]["min refinements"][data["min refinements"].get<int>()].get<int>();
				}

				if (data.contains("max refinements") && data["max refinements"].is_number_integer())
				{
					config.refinements_max = perf_data["scene"]["model"]["material"]["ssr"]["max refinements"][data["max refinements"].get<int>()].get<int>();
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

				std::shared_ptr<Model> model = std::make_shared<Model>(scene->GetNewModelReference(), std::vector<std::shared_ptr<Geometry>>(), scene.get());

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
				model->GetColourTexture() = GetTexture(el.value()["textures"]["colour"], config.path.root, scene->GetNewTextureReference(), glm::vec3(1.0f), TextureFiltering::Linear, TextureFiltering::Linear);
				model->GetNormalTexture() = GetTexture(el.value()["textures"]["normal"], config.path.root, scene->GetNewTextureReference(), glm::vec3(0.5f, 0.5f, 1.0f), TextureFiltering::Linear, TextureFiltering::Linear);
				model->GetReflectionTexture() = GetTexture(el.value()["textures"]["reflection intensity"], config.path.root, scene->GetNewTextureReference(), glm::vec3(0.0f), TextureFiltering::Linear, TextureFiltering::Linear);
				model->GetSpecularTexture() = GetTexture(el.value()["textures"]["specular"], config.path.root, scene->GetNewTextureReference(), glm::vec3(0.0f), TextureFiltering::Linear, TextureFiltering::Linear);
				model->GetSkyboxMaskTexture() = GetTexture(el.value()["textures"]["skybox mask"], config.path.root, scene->GetNewTextureReference(), glm::vec3(0.0f), TextureFiltering::Nearest, TextureFiltering::Nearest);
				model->GetDisplacementTexture() = GetTexture(el.value()["textures"]["displacement"], config.path.root, scene->GetNewTextureReference(), glm::vec3(0.0f), TextureFiltering::Linear, TextureFiltering::Linear);

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
					std::optional<std::shared_ptr<VisBox>> visbox = scene->GetVisBox(el2);

					if (visbox.has_value())
					{
						visbox.value()->AddMemberModel(model);
					}
					else
					{
						throw std::runtime_error("Unknown PVS identifier '" + el2 + "' used when specifying model PVS membership");
					}
				}

				scene->Add(model);
				models.push_back(model);
			}
		}
	}

	std::thread geometry_loader_thread = std::thread([config, scene, scene_data]()
		{
			NameThread(L"Model loader");
			ModelLoader loader = ModelLoader(config.path.root.string(), scene_data["layout"], scene_data["models"], scene);
		});

	//load all reflections with model ptrs
	for (auto& el : scene_data["reflections"].items())
	{
		std::shared_ptr<Reflection> reflection = std::make_shared<Reflection>(scene->GetNewRenderTextureReference());

		reflection->SetPosition(GetVector(el.value()["position"], glm::dvec3(0.0)));

		if (el.value()["identifier"].is_string())
		{
			reflection->SetIdentifier(el.value()["identifier"].get<std::string>());
		}

		if (el.value()["draw shadows"].is_boolean())
		{
			reflection->SetDrawShadows(el.value()["draw shadows"].get<bool>()); 
		}

		if (el.value()["draw reflections"].is_boolean())
		{
			reflection->SetDrawReflections(el.value()["draw reflections"].get<bool>());
		}

		ConfigureCubemap(el.value(), perf_data, reflection.get(), scene);

		scene->Add(reflection);
	}

	//load reflection ptrs into all models that require them for reflections
	if (scene_data["layout"].is_array())
	{
		for (int i = 0; i < (int)scene_data["layout"].size(); i++)
		{
			auto& el = scene_data["layout"][i];
			std::shared_ptr<Model> model = models.at(i);

			if (el["reflections"].is_object() && el["reflections"]["alternative"].is_object())
			{
				model->GetMaterial().reflections_enabled = true;

				ReflectionMode mode;
				if (el["reflections"]["alternative"]["mode"].is_string())
				{
					std::string mode_name = el["reflections"]["alternative"]["mode"].get<std::string>();
					if (mode_name == "simple")
					{
						mode = ReflectionMode::Simple;
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
					std::optional<std::shared_ptr<Reflection>> reflection = scene->GetReflection(el2);
					if (reflection.has_value())
					{
						model->GetMaterial().reflections.push_back(std::tuple(reflection.value(), mode));
					}
					else
					{
						throw std::runtime_error("Couldn't resolve alternative reflection for model with identifier '" + el2 + "'");
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
				std::shared_ptr<PointLight> pointlight = std::make_shared<PointLight>(scene->GetNewRenderTextureReference());

				pointlight->SetPosition(GetVector(el.value()["position"], glm::dvec3(0.0)));
				pointlight->SetIntensity(GetVector(el.value()["intensity"], glm::dvec3(0.0)));

				if (el.value()["shadows"].is_object())
				{
					if (el.value()["shadows"]["acceptance bias"].is_number())
					{
						pointlight->SetShadowBias(el.value()["shadows"]["acceptance bias"].get<double>());
					}

					ConfigureCubemap(el.value()["shadows"], perf_data, pointlight.get(), scene);
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
				std::shared_ptr<Skybox> skybox = std::make_shared<Skybox>(scene->GetNewRenderTextureReference());
				skybox->SetPosition(GetVector(el.value()["position"], glm::dvec3(0.0)));

				ConfigureCubemap(el.value(), perf_data, skybox.get(), scene);

				scene->Add(skybox);

				for (auto& el2 : el.value()["drawn on"].items())
				{
					if (el2.value().is_string())
					{
						std::optional<std::shared_ptr<Model>> model = scene->GetModel(el2.value().get<std::string>());

						if (model.has_value())
						{
							model.value()->SetSkybox(skybox);
						}
						else
						{
							throw std::runtime_error("Invalid model identifier \"" + el2.value().get<std::string>() + "\"");
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

	return std::tuple(scene, std::move(geometry_loader_thread));
}
