#pragma once

#include <vector>

#include "Model.h"
#include "Camera.h"
#include "render/EngineCanvas.h"

class EngineCanvas;

class Scene
{
private:
	Camera* m_active_camera;

	int GetModelIndex(Model* model);
	int GetCameraIndex(Camera* camera);
	
public:
	Scene(Camera* active_camera);
	~Scene();

	std::vector<Model*> models;
	std::vector<Camera*> cameras;

	void AddModel(Model* model);
	void RemoveModel(Model* model);
	void AddCamera(Camera* camera);
	void RemoveCamera(Camera* camera);
	void SetActiveCamera(Camera* camera);
	Camera* GetActiveCamera();

	size_t NumModels();
	size_t NumCameras();

	void ClearAllModels(bool destroy = false);
	void ClearAllCameras(bool destroy = false);

	void Render(EngineCanvas* canvas);
};