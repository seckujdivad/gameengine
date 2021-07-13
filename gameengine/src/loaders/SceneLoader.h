#pragma once

#include <filesystem>
#include <tuple>
#include <thread>

class Scene;

struct SceneLoaderConfig
{
	struct Path
	{
		std::filesystem::path root = "";
		std::filesystem::path file = "";
	} path;
	
	struct Performance
	{
		int index = 0;
	} performance;
};

std::tuple<std::shared_ptr<Scene>, std::thread> SceneFromJSON(SceneLoaderConfig config);