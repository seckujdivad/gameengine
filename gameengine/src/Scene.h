#pragma once

#include <wx/wxprec.h>
#include "GLComponents.h"

#include <vector>

#include "render/ShaderProgram.h"
#include "Model.h"
#include "Camera.h"
#include "render/EngineCanvas.h"

class EngineCanvas;

class Scene
{
private:
	Camera* m_active_camera;
	
	std::string m_identifier;

	glm::vec3 m_light_ambient = glm::vec3(0.0f, 0.0f, 0.0f);

	int GetModelIndex(Model* model);
	int GetCameraIndex(Camera* camera);
	
public:
	Scene(Camera* active_camera);
	~Scene();

	void SetIdentifier(std::string identifier);
	std::string GetIdentifier();

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

	void PushUniforms();

	void SetAmbientLight(glm::vec3 light_intensity);
};