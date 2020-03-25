#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <vector>

#include "../render/ShaderProgram.h"
#include "model/Model.h"
#include "Camera.h"
#include "../render/EngineCanvas.h"
#include "light/PointLight.h"

class EngineCanvas;

class Scene
{
private:
	Camera* m_active_camera;
	
	std::string m_identifier;

	glm::vec3 m_light_ambient = glm::vec3(0.0f, 0.0f, 0.0f);

	int GetModelIndex(Model* model);
	int GetCameraIndex(Camera* camera);
	int GetPointLightIndex(PointLight* pointlight);
	
public:
	Scene(Camera* active_camera);
	~Scene();

	void SetIdentifier(std::string identifier);
	std::string GetIdentifier();

	std::vector<Model*> models;
	std::vector<Camera*> cameras;
	std::vector<PointLight*> pointlights;

	void AddModel(Model* model);
	void RemoveModel(Model* model);
	void AddCamera(Camera* camera);
	void RemoveCamera(Camera* camera);
	void SetActiveCamera(Camera* camera);
	Camera* GetActiveCamera();
	void AddPointLight(PointLight* pointlight);
	void RemovePointLight(PointLight* pointlight);

	size_t NumModels();
	size_t NumCameras();

	void ClearAllModels(bool destroy = false);
	void ClearAllCameras(bool destroy = false);

	void Render(EngineCanvas* canvas);

	void PushUniforms();

	void SetAmbientLight(glm::vec3 light_intensity);
};