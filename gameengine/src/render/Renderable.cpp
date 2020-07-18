#include "Renderable.h"

void Renderable::RenderScene()
{
	//set preprocessor defines

	if (this->m_rendermode == RenderMode::Normal)
	{
		//draw shadows and reflections (if required)
	}

	glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
	glCullFace(GL_BACK);
	glViewport(0, 0, std::get<0>(this->GetOutputSize()), std::get<1>(this->GetOutputSize()));
	glClearColor(
		this->m_scene->GetClearColour().r,
		this->m_scene->GetClearColour().g,
		this->m_scene->GetClearColour().b,
		this->m_scene->GetClearColour().a
	);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	//load camera data into shader program

	std::vector<Model*> models_to_draw = this->m_scene->GetVisibleModels(this->m_active_camera->GetPositionVec());

	//iterate through models
	// load model data into shader program
	// load model textures into shader program
	// draw model
}

void Renderable::RecompileShader()
{
	if (this->m_shader_program == nullptr)
	{
		delete this->m_shader_program;
	}

	this->m_shader_program = new ShaderProgram(
		this->m_shaders,
		this->m_shader_defines,
		false
	);

	std::vector<std::string> uniform_names = {

	};

	for (std::vector<std::string>::iterator it = uniform_names.begin(); it != uniform_names.end(); it++)
	{
		this->m_shader_program->RegisterUniform(*it);
	}
}

void Renderable::SetFramebuffer(GLuint fbo)
{
	this->m_fbo = fbo;
}

void Renderable::RenderInitialisationEvent()
{
}

void Renderable::PreRenderEvent()
{
}

void Renderable::PostRenderEvent()
{

}

Renderable::Renderable(Scene* scene, std::vector<std::tuple<std::string, GLenum>> shaders)
{
	this->m_scene = scene;
	this->m_shaders = shaders;

	this->RecompileShader();
}

Renderable::~Renderable()
{
	delete this->m_shader_program;
}

Scene* Renderable::GetScene()
{
	return this->m_scene;
}

void Renderable::SetActiveCamera(Camera* camera)
{
	this->m_active_camera = camera;
}

Camera* Renderable::GetActiveCamera()
{
	return this->m_active_camera;
}

void Renderable::Render(bool continuous_draw)
{
	if (this->m_scene != nullptr)
	{
		this->RenderInitialisationEvent();

		if (continuous_draw)
		{
			glFlush();
			this->PostRenderEvent();
		}
		else
		{
			this->PreRenderEvent();
		}

		std::tuple<int, int> sizes = this->GetOutputSize();

		glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
		glViewport(0, 0, std::get<0>(sizes), std::get<0>(sizes));
		this->RenderScene();

		if (continuous_draw)
		{
			this->PreRenderEvent();
		}
		else
		{
			glFlush();
			this->PostRenderEvent();
		}
	}
}

std::tuple<int, int> Renderable::GetOutputSize()
{
	throw std::logic_error("Method must be overridden");
}


void Renderable::SetRenderMode(RenderMode mode)
{
	this->m_rendermode = mode;
}

RenderMode Renderable::GetRenderMode()
{
	return this->m_rendermode;
}