#pragma once

#include <filesystem>
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

std::thread SceneFromJSON(Scene& scene, SceneLoaderConfig config);