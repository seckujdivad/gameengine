#include "ModelLoader.h"

#include "BptLoader.h"
#include "PlyLoader.h"
#include "../GetVector.h"
#include "../../scene/model/Model.h"
#include "../../scene/model/geometry/Geometry.h"
#include "../../scene/model/geometry/Polygonal.h"
#include "../../scene/Scene.h"
#include "../../generic/ThreadNamer.h"

void ModelLoader::GreedyLoadGeometry(std::size_t identifier) //take the newest geometry name, load the associated geometry objects and then feed them back to the main thread to be given to Models
{
	NameThread(L"Greedy Geometry Loader");

	bool has_geometry_to_load = true;
	while (has_geometry_to_load)
	{
		std::unique_lock<std::mutex> lock = std::unique_lock(this->m_to_load_mutex);
		if (this->m_to_load.size() > 0)
		{
			std::string geometry_name = this->m_to_load.front();
			this->m_to_load.pop();
			lock.unlock();

			std::vector<std::shared_ptr<Geometry>> geometry = this->LoadGeometry(geometry_name);
			std::unique_lock<std::mutex> lock = std::unique_lock(this->m_loaded_mutex);
			this->m_loaded.push(ModelLoader::LoadedGeometry(geometry_name, geometry));
			this->m_loaded_has_waiting_data.notify_all();
		}
		else
		{
			has_geometry_to_load = false;
		}
	}

	std::unique_lock<std::mutex> lock = std::unique_lock(this->m_to_load_mutex);
	this->m_loaded.push(identifier);
}

std::vector<std::shared_ptr<Geometry>> ModelLoader::LoadGeometry(std::string geometry_name) //load some geometry objects by name
{
	std::vector<std::shared_ptr<Geometry>> result;
	if (this->m_geom_info["ply"][geometry_name].is_object())
	{
		nlohmann::json& config = this->m_geom_info["ply"][geometry_name];
		std::string path = (this->m_root_path / config["path"].get<std::string>()).string();

		std::shared_ptr<Polygonal> model_geometry = ModelFromPly(path);

		if (config["merge geometry"].is_object())
		{
			if (config["merge geometry"]["enable"].is_boolean() && config["merge geometry"]["distance"].is_number())
			{
				if (config["merge geometry"]["enable"].get<bool>())
				{
					double merge_distance = config["merge geometry"]["distance"].get<double>();
					if (merge_distance >= 0.0)
					{
						model_geometry->MergeVertices(merge_distance);
					}
					else
					{
						throw std::invalid_argument("Merge distance is less than 0 (" + std::to_string(merge_distance) + ")");
					}
				}
			}
			else
			{
				throw std::invalid_argument("Merge geometry field must contain 'enable' (bool) and 'distance' (number)");
			}
		}

		if (config["invert normals"].is_boolean())
		{
			if (config["invert normals"].get<bool>())
			{
				model_geometry->InvertNormals();
			}
		}

		model_geometry->SnapVerticesToGrid(GetVector(config["grid"], glm::dvec3(0.0)));

		result.push_back(std::dynamic_pointer_cast<Geometry>(model_geometry));
	}

	if (this->m_geom_info["bpt"][geometry_name].is_object())
	{
		const auto& config = this->m_geom_info["bpt"][geometry_name];

		std::vector<std::shared_ptr<Patch>> patches = PatchesFromBPT((this->m_root_path / config["path"].get<std::string>()).string());

		result.reserve(result.size() + patches.size());
		for (const std::shared_ptr<Patch>& patch : patches)
		{
			result.push_back(std::dynamic_pointer_cast<Geometry>(patch));
		}
	}

	return result;
}

void ModelLoader::SendToModels(std::string geometry_name, std::vector<std::shared_ptr<Geometry>> geometry) //attach geometry to all models that require geometry of the given UID
{
	if (this->m_models.count(geometry_name) > 0)
	{
		const std::vector<std::shared_ptr<Model>>& models = this->m_models.at(geometry_name);
		for (const std::shared_ptr<Model>& model : models)
		{
			std::unique_lock<std::mutex> lock = std::unique_lock(this->m_scene->GetMutex());
			model->AddGeometry(geometry);
		}
	}
}

void ModelLoader::DistributeOutstandingGeometry()
{
	std::vector<std::variant<std::size_t, LoadedGeometry>> load_results;
	while (this->m_loaded.size() > 0)
	{
		load_results.push_back(this->m_loaded.front());
		this->m_loaded.pop();
	}

	for (const std::variant<std::size_t, LoadedGeometry>& load_result : load_results)
	{
		if (load_result.index() == 0)
		{
			const std::size_t& uid = std::get<std::size_t>(load_result);
			this->m_loaders.at(uid)->join();
			this->m_loaders.erase(uid);
		}
		else
		{
			this->SendToModels(std::get<LoadedGeometry>(load_result).name, std::get<LoadedGeometry>(load_result).geometry);
		}
	}
}

ModelLoader::ModelLoader(std::string root, nlohmann::json layout, nlohmann::json geometry_info, std::shared_ptr<Scene> scene) : m_layout(layout), m_geom_info(geometry_info), m_root_path(root), m_scene(scene)
{
	//get the shared_ptrs for all the models and the Geometry that they all require
	for (auto it = layout.begin(); it != layout.end(); ++it)
	{
		std::string identifier = it.value()["identifier"].get<std::string>();

		std::shared_ptr<Model> model = nullptr;
		for (const std::shared_ptr<Model>& model_candidate : scene->GetModels())
		{
			if (model_candidate->GetIdentifier() == identifier)
			{
				model = model_candidate;
			}
		}

		if (model.get() == nullptr)
		{
			throw std::invalid_argument("Model \"" + identifier + "\" has not been loaded");
		}
		else if (it.value().is_object())
		{
			std::string err_string = "Model \"" + identifier + "\" must contain one of \"model\" or \"models\" which must be a string or list of strings";

			//load the model Geometry name strings
			std::vector<std::string> names;
			bool is_valid = false;

			if (it.value()["model"].is_string())
			{
				names.push_back(it.value()["model"].get<std::string>());
				is_valid = true;
			}

			if (it.value()["model"].is_array())
			{
				if (is_valid)
				{
					throw std::invalid_argument(err_string);
				}
				else
				{
					for (auto it2 = it.value()["model"].begin(); it2 != it.value()["model"].end(); ++it2)
					{
						names.push_back(it2.value().get<std::string>());
					}
					is_valid = true;
				}
			}

			if (it.value()["models"].is_string())
			{
				if (is_valid)
				{
					throw std::invalid_argument(err_string);
				}
				else
				{
					names.push_back(it.value()["models"].get<std::string>());
					is_valid = true;
				}
			}

			if (it.value()["models"].is_array())
			{
				if (is_valid)
				{
					throw std::invalid_argument(err_string);
				}
				else
				{
					for (auto it2 = it.value()["models"].begin(); it2 != it.value()["models"].end(); ++it2)
					{
						names.push_back(it2.value().get<std::string>());
					}
					is_valid = true;
				}
			}

			if (is_valid)
			{
				for (const std::string& geometry_name : names)
				{
					if (this->m_models.count(geometry_name) == 0)
					{
						this->m_models.insert(std::pair(geometry_name, std::vector<std::shared_ptr<Model>>()));
					}
					this->m_models.at(geometry_name).push_back(model);

					this->m_to_load.push(geometry_name);
				}
			}
			else
			{
				throw std::invalid_argument(err_string);
			}
		}
		else
		{
			throw std::invalid_argument("Model \"" + it.key() + "\" must be an object");
		}
	}

	//create thread pool
	std::size_t thread_pool_size = std::min(std::thread::hardware_concurrency(), this->m_to_load.size());
	if (thread_pool_size > 0)
	{
		for (std::size_t i = 0; i < thread_pool_size; i++)
		{
			std::size_t uid = this->m_loader_counter++;
			this->m_loaders.insert(std::pair(uid, std::make_unique<std::thread>(std::bind(&ModelLoader::GreedyLoadGeometry, this, uid))));
		}

		//await results from thread pool
		while (this->m_loaders.size() > 0)
		{
			std::unique_lock<std::mutex> lock = std::unique_lock(this->m_loaded_mutex);
			this->m_loaded_has_waiting_data.wait(lock);

			this->DistributeOutstandingGeometry();
		}

		std::unique_lock<std::mutex> lock = std::unique_lock(this->m_loaded_mutex);
		this->DistributeOutstandingGeometry();
	}
}

ModelLoader::~ModelLoader()
{
	//ensure all threads exit
	for (const auto& [uid, thread] : this->m_loaders)
	{
		thread->join();
	}
}

ModelLoader::LoadedGeometry::LoadedGeometry(std::string name, std::vector<std::shared_ptr<Geometry>> geometry) : name(name), geometry(geometry)
{
}
