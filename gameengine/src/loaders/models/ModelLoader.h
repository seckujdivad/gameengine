#pragma once

#include <vector>
#include <string>
#include <condition_variable>
#include <thread>
#include <mutex>
#include <memory>
#include <tuple>
#include <unordered_map>
#include <queue>
#include <filesystem>
#include <variant>

#include <nlohmann/json.hpp>

class Model;
class Geometry;
class Scene;

class ModelLoader
{
private:
	//scene data
	nlohmann::json m_layout;
	nlohmann::json m_geom_info;
	std::filesystem::path m_root_path;

	Scene& m_scene;

	std::unordered_map<std::string, std::vector<std::shared_ptr<Model>>> m_models;

	//thread data
	std::size_t m_loader_counter = 0;
	std::unordered_map<std::size_t, std::unique_ptr<std::thread>> m_loaders;

	// data to load
	std::queue<std::string> m_to_load;
	std::mutex m_to_load_mutex;

	// loaded data
	struct LoadedGeometry
	{
		LoadedGeometry(std::string name, std::vector<std::shared_ptr<Geometry>> geometry);

		std::string name;
		std::vector<std::shared_ptr<Geometry>> geometry;
	};

	std::queue<std::variant<std::size_t, LoadedGeometry>> m_loaded;
	std::mutex m_loaded_mutex;
	std::condition_variable m_loaded_has_waiting_data;

	void GreedyLoadGeometry(std::size_t identifier);
	std::vector<std::shared_ptr<Geometry>> LoadGeometry(std::string geometry_name);

	void SendToModels(std::string geometry_name, std::vector<std::shared_ptr<Geometry>> geometry);
	void DistributeOutstandingGeometry();

public:
	ModelLoader(std::string root, nlohmann::json layout, nlohmann::json geometry_info, Scene& scene);
	ModelLoader(const ModelLoader&) = delete;
	ModelLoader& operator=(const ModelLoader&) = delete;
	ModelLoader(ModelLoader&&) = delete;
	ModelLoader& operator=(ModelLoader&&) = delete;
	~ModelLoader();
};