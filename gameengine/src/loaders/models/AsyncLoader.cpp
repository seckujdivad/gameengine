#include "AsyncLoader.h"

#include <thread>
#include <memory>
#include <mutex>
#include <queue>
#include <string>
#include <tuple>
#include <filesystem>
#include <condition_variable>
#include <unordered_map>

#include "BptLoader.h"
#include "PlyLoader.h"
#include "../GetVector.h"
#include "../../scene/model/Model.h"
#include "../../scene/model/geometry/Geometry.h"
#include "../../scene/model/geometry/Polygonal.h"

void LoadModelGeometry(std::vector<std::shared_ptr<Model>> models, std::string root, nlohmann::json layout, nlohmann::json geometry_info)
{
	//in a new thread:
	// create task queue, one item for each piece of geometry
	// create results queue
	// create appropriately sized thread pool
	//  each thread in the pool takes the next task and works on it
	//  when it is done, it will check for more tasks. if none, exit
	// main thread reads off results and adds them to models until all threads terminate
	
	//need: synchronisation primitives for both queues
	std::thread(LoadModelGeometryThread, root, models, layout, geometry_info);
}

void LoadModelGeometryThread(std::string root, std::vector<std::shared_ptr<Model>> models, nlohmann::json layout, nlohmann::json geometry_info)
{
	std::unordered_map<std::string, std::vector<std::shared_ptr<Model>>> model_lookup;
	for (auto it = layout.begin(); it != layout.end(); ++it)
	{
		std::shared_ptr<Model> model = nullptr;
		for (const std::shared_ptr<Model>& model_candidate : models)
		{
			if (model_candidate->GetIdentifier() == it.key())
			{
				model = model_candidate;
			}
		}

		if (model.get() == nullptr)
		{
			throw std::invalid_argument("Model \"" + it.key() + "\" has not been loaded");
		}
		else if (it.value().is_object())
		{
			std::string err_string = "Model \"" + it.key() + "\" must contain one of \"model\" or \"models\" which must be a string or list of strings";

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
						names.push_back(it.value()["model"].get<std::string>());
					}
					is_valid = true;
				}
			}

			if (it.value()["model"].is_string())
			{
				if (is_valid)
				{
					throw std::invalid_argument(err_string);
				}
				else
				{
					names.push_back(it.value()["model"].get<std::string>());
					is_valid = true;
				}
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
						names.push_back(it.value()["model"].get<std::string>());
					}
					is_valid = true;
				}
			}
			
			if (is_valid)
			{
				for (const auto& geometry_name : names)
				{
					if (model_lookup.count(geometry_name) == 0)
					{
						model_lookup.insert(std::pair(geometry_name, std::vector<std::shared_ptr<Model>>()));
					}
					model_lookup.at(geometry_name).push_back(model);
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

	std::mutex to_load_lock;
	std::queue<std::string> to_load;

	std::tuple<std::mutex, std::condition_variable> output_lock;
	std::queue<std::tuple<std::string, std::vector<std::shared_ptr<Geometry>>>> output;

	std::atomic<int> output_size = 0;

	if (geometry_info["ply"].is_object())
	{
		for (auto it = geometry_info["ply"].begin(); it != geometry_info["ply"].end(); ++it)
		{
			to_load.push(it.key());
		}
	}

	if (geometry_info["bpt"].is_object())
	{
		for (auto it = geometry_info["bpt"].begin(); it != geometry_info["bpt"].end(); ++it)
		{
			to_load.push(it.key());
		}
	}

	std::size_t thread_pool_size = std::min(std::thread::hardware_concurrency(), to_load.size());
	if (thread_pool_size > 0)
	{
		std::vector<std::tuple<std::mutex, std::thread>> thread_pool;
		thread_pool.reserve(thread_pool_size);
		for (std::size_t i = 0; i < thread_pool_size; i++)
		{
			std::mutex is_running_lock;
			thread_pool.emplace_back(std::move(is_running_lock), std::thread(LoadGeometry, root, geometry_info, to_load, to_load_lock, output, output_lock, output_size, is_running_lock));
		}

		bool continue_waiting = true;
		while (continue_waiting)
		{
			std::unique_lock<std::mutex> lock = std::unique_lock(std::get<0>(output_lock));
			std::get<1>(output_lock).wait(lock); //lock is released as soon as this function starts waiting on the std::condition_variable
			lock.lock();

			while (output.size() > 0)
			{
				const auto& [name, geometries] = output.front();

				const std::vector<std::shared_ptr<Model>>& models = model_lookup.at(name);
				for (const std::shared_ptr<Model>& model : models)
				{
					std::unique_lock<std::mutex> geometry_lock = std::unique_lock(model->GetGeometryMutex());
					for (const std::shared_ptr<Geometry>& geometry : geometries)
					{
						model->AddGeometry(geometry);
					}
				}

				output.pop();
			}

			lock.unlock();

			for (std::size_t i = 0; i < thread_pool.size();)
			{
				auto& [running_mutex, thread] = thread_pool.at(i);
				if (running_mutex.try_lock())
				{
					running_mutex.unlock();
					thread.join();
					thread_pool.erase(thread_pool.begin() + i);
				}
				else
				{
					i++;
				}
			}

			continue_waiting = thread_pool.size() > 0;
		}
	}
}

void LoadGeometry(std::filesystem::path root, nlohmann::json geometry, std::queue<std::string>& to_load, std::mutex& to_load_lock, std::queue<std::tuple<std::string, std::vector<std::shared_ptr<Geometry>>>>& output, std::tuple<std::mutex, std::condition_variable>& output_lock, std::mutex& is_running_lock)
{
	std::unique_lock<std::mutex> is_running_lock = std::unique_lock(is_running_lock);

	to_load_lock.lock();
	while (!to_load.empty())
	{
		std::string name = to_load.front();
		to_load.pop();
		to_load_lock.unlock();

		//load
		std::vector<std::shared_ptr<Geometry>> result;
		if (geometry["ply"][name].is_object())
		{
			const auto& config = geometry["ply"][name];
			std::string path = (root / config["path"].get<std::string>()).string();

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
					throw std::invalid_argument("Merge geometry field must contain 'enabled' (bool) and 'distance' (number)");
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

		if (geometry["bpt"][name].is_object())
		{
			const auto& config = geometry["bpt"][name];

			std::vector<std::shared_ptr<Patch>> patches = PatchesFromBPT((root / config["path"].get<std::string>()).string());

			result.reserve(result.size() + patches.size());
			for (const std::shared_ptr<Patch>& patch : patches)
			{
				result.push_back(std::dynamic_pointer_cast<Geometry>(patch));
			}
		}

		std::get<0>(output_lock).lock();
		output.emplace(name, result);
		std::get<1>(output_lock).notify_all();
		std::get<0>(output_lock).unlock();

		to_load_lock.lock();
	}
	to_load_lock.unlock();
}