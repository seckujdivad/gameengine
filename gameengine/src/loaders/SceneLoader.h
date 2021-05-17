#pragma once

#include <filesystem>

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

Scene* SceneFromJSON(SceneLoaderConfig config);