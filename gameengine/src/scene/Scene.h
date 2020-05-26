#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <vector>
#include <unordered_set>

#include "../render/ShaderProgram.h"
#include "model/Model.h"
#include "Camera.h"
#include "light/PointLight.h"
#include "model/Reflection.h"
#include "VisBox.h"
#include "SceneApproximation.h"
#include "../render/Renderable.h"

struct ShaderDescription
{
	std::vector<std::tuple<std::string, GLenum>> shaders;
	std::vector<std::tuple<std::string, std::string>> preprocessor_defines;
	bool shader_strings_are_paths = true;
};

class Scene : public Nameable
{
private:
	glm::vec3 m_light_ambient = glm::vec3(0.0f, 0.0f, 0.0f);

	glm::vec4 m_clear_colour = glm::vec4(1.0f, 1.0f, 1.0f, 1.0f);

	Scene* m_skybox_scene = nullptr;
	GLuint m_skybox_texture = NULL;
	unsigned int m_skybox_texture_dimensions[2] = { 1, 1 };
	GLuint m_skybox_fbo = NULL;

	//received render outputs
	GLuint m_output_colour = NULL;
	GLuint m_output_depth = NULL;
	std::vector<GLuint> m_output_data;

	int GetModelIndex(Model* model);
	int GetCameraIndex(Camera* camera);
	int GetPointLightIndex(PointLight* pointlight);
	int GetReflectionIndex(Reflection* reflection);
	int GetVisBoxIndex(VisBox* visbox);

	//managed shaders
	std::vector<ShaderDescription> m_shader_descriptions;
	std::vector<ShaderProgram*> m_shader_programs;

	//approximate scene representation
	SceneApproximation* m_approximation = nullptr;

	int m_mode = 0;
	
public:
	Scene(int mode = 0);
	~Scene();

	std::vector<Model*> models;
	std::vector<Camera*> cameras;
	std::vector<PointLight*> pointlights;
	std::vector<Reflection*> reflections;
	std::vector<VisBox*> visboxes;

	void AddModel(Model* model);
	void RemoveModel(Model* model);

	void AddCamera(Camera* camera);
	void RemoveCamera(Camera* camera);

	void AddPointLight(PointLight* pointlight);
	void RemovePointLight(PointLight* pointlight);

	void AddReflection(Reflection* reflection);
	void RemoveReflection(Reflection* reflection);

	void AddVisBox(VisBox* visbox);
	void RemoveVisBox(VisBox* visbox);
	
	Nameable* GetByIdentifier(std::string identifier, int type); //types: 0 - model, 1 - camera, 2 - point light, 3 - reflection, 4 - vis box

	size_t NumModels();
	size_t NumCameras();

	void ClearAllModels(bool destroy = false);
	void ClearAllCameras(bool destroy = false);

	void Render(GLuint framebuffer, Camera* camera); //You must set the viewport dimensions before calling. Defaults to the default framebuffer
	void DrawShadows(int mode = 0); //0: static, 1: dynamic
	void DrawReflections(int mode = 0); //0: static, 1: dynamic

	void PushUniforms();

	void SetAmbientLight(glm::vec3 light_intensity);

	void InitialiseSkyboxTexture(unsigned int texture_width, unsigned int texture_height);
	void SetSkyboxScene(Scene* scene);
	void DrawSkyboxScene();

	void SetClearColour(glm::vec4 colour);
	glm::vec4 GetClearColour();

	std::vector<Model*> GetVisibleModels(glm::vec3 position);

	void SetReceivedOutputTextures(GLuint colour, GLuint depth, std::vector<GLuint> data);

	ShaderProgram* GetShaderProgram(ShaderDescription description);

	void SetApproximation(SceneApproximation* approximation);
};