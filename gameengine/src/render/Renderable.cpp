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
		{
			{this->m_shader_fragment, GL_FRAGMENT_SHADER},
			{this->m_shader_vertex, GL_VERTEX_SHADER}
		},
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

Renderable::Renderable(Scene* scene, std::string vert_shader, std::string frag_shader)
{
	this->m_scene = scene;

	this->m_shader_fragment = frag_shader;
	this->m_shader_vertex = vert_shader;

	this->RecompileShader();
}

Renderable::~Renderable()
{
	if (this->m_postprocessor != nullptr)
	{
		delete this->m_postprocessor;

		glDeleteTextures(1, &this->m_postprocessor_colour_texture_write);
		glDeleteTextures(1, &this->m_postprocessor_depth_texture_write);
		glDeleteTextures((GLsizei)this->m_postprocessor_data_textures_write.size(), this->m_postprocessor_data_textures_write.data());

		glDeleteTextures(1, &this->m_postprocessor_colour_texture_read);
		glDeleteTextures(1, &this->m_postprocessor_depth_texture_read);
		glDeleteTextures((GLsizei)this->m_postprocessor_data_textures_read.size(), this->m_postprocessor_data_textures_read.data());

		glDeleteFramebuffers(1, &this->m_postprocessor_fbo);
	}
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

		this->m_scene->SetReceivedOutputTextures(this->m_postprocessor_colour_texture_read, this->m_postprocessor_depth_texture_read, this->m_postprocessor_data_textures_read);

		std::tuple<int, int> sizes = this->GetOutputSize();
		int tex_size[2] = { std::get<0>(sizes), std::get<1>(sizes) };

		if (this->m_postprocessor == nullptr)
		{
			glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
			glViewport(0, 0, tex_size[0], tex_size[1]);
			this->RenderScene();
		}
		else
		{
			if ((this->m_old_size[0] != tex_size[0]) || (this->m_old_size[1] != tex_size[1]))
			{
				glBindFramebuffer(GL_FRAMEBUFFER, this->m_postprocessor_fbo);

				glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_colour_texture_write);
				glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_size[0], tex_size[1], 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);

				glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_depth_texture_write);
				glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, tex_size[0], tex_size[1], 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
				glViewport(0, 0, tex_size[0], tex_size[1]);

				for (int i = 0; i < (int)this->m_postprocessor_data_textures_write.size(); i++)
				{
					glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_data_textures_write.at(i));
					glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_size[0], tex_size[1], 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
				}

				glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_colour_texture_read);
				glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_size[0], tex_size[1], 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);

				glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_depth_texture_read);
				glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, tex_size[0], tex_size[1], 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
				glViewport(0, 0, tex_size[0], tex_size[1]);

				for (int i = 0; i < (int)this->m_postprocessor_data_textures_read.size(); i++)
				{
					glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_data_textures_read.at(i));
					glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_size[0], tex_size[1], 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
				}

				this->m_old_size[0] = tex_size[0];
				this->m_old_size[1] = tex_size[1];
			}

			this->RenderScene();

			glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
			glViewport(0, 0, tex_size[0], tex_size[1]);
			glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

			this->m_postprocessor->Select();
			glBindVertexArray(this->m_postprocessor_vao);
			glDrawArrays(GL_TRIANGLES, 0, 6);

			//copy textures from the render textures to the readable textures
			glCopyImageSubData(this->m_postprocessor_colour_texture_write, GL_TEXTURE_2D, 0, 0, 0, 0,
				this->m_postprocessor_colour_texture_read, GL_TEXTURE_2D, 0, 0, 0, 0,
				tex_size[0], tex_size[1], 1);
			glCopyImageSubData(this->m_postprocessor_depth_texture_write, GL_TEXTURE_2D, 0, 0, 0, 0,
				this->m_postprocessor_depth_texture_read, GL_TEXTURE_2D, 0, 0, 0, 0,
				tex_size[0], tex_size[1], 1);
			for (int i = 0; i < (int)this->m_postprocessor_data_textures_read.size(); i++)
			{
				glCopyImageSubData(this->m_postprocessor_data_textures_write.at(i), GL_TEXTURE_2D, 0, 0, 0, 0,
					this->m_postprocessor_data_textures_read.at(i), GL_TEXTURE_2D, 0, 0, 0, 0,
					tex_size[0], tex_size[1], 1);
			}
		}

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

void Renderable::SetPostProcessorShaderProgram(ShaderProgram* postprocessor)
{
	this->m_postprocessor = postprocessor;

	if (this->m_postprocessor_vao == NULL)
	{
		//create preprocessor vertices
		float vertices[] = {
			-1.0f, -1.0f, 0.0f,
			1.0f, -1.0f, 0.0f,
			1.0f, 1.0f, 0.0f,
			-1.0f, -1.0f, 0.0f,
			1.0f, 1.0f, 0.0f,
			-1.0f, 1.0f, 0.0f
		};
		glGenVertexArrays(1, &this->m_postprocessor_vao);
		glBindVertexArray(this->m_postprocessor_vao);
		glGenBuffers(1, &this->m_postprocessor_vbo);
		glBindBuffer(GL_ARRAY_BUFFER, this->m_postprocessor_vbo);
		glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), vertices, GL_STATIC_DRAW);
		glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * sizeof(float), (void*)0);
		glEnableVertexAttribArray(0);
	}

	std::tuple<int, int> sizes = this->GetOutputSize();
	int tex_size[2] = { std::get<0>(sizes), std::get<1>(sizes) };

	//writeable textures (rendered to)
	glGenTextures(1, &this->m_postprocessor_colour_texture_write);
	glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_colour_texture_write);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_size[0], tex_size[1], 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	glGenTextures(1, &this->m_postprocessor_depth_texture_write);
	glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_depth_texture_write);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, tex_size[0], tex_size[1], 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	for (int i = 0; i < ENGINECANVAS_NUM_DATA_TEX; i++)
	{
		GLuint texture_id;
		glGenTextures(1, &texture_id);
		glBindTexture(GL_TEXTURE_2D, texture_id);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_size[0], tex_size[1], 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

		this->m_postprocessor_data_textures_write.push_back(texture_id);
	}

	//readable textures (rendered from)
	glGenTextures(1, &this->m_postprocessor_colour_texture_read);
	glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_colour_texture_read);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_size[0], tex_size[1], 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	glGenTextures(1, &this->m_postprocessor_depth_texture_read);
	glBindTexture(GL_TEXTURE_2D, this->m_postprocessor_depth_texture_read);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, tex_size[0], tex_size[1], 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	for (int i = 0; i < ENGINECANVAS_NUM_DATA_TEX; i++)
	{
		GLuint texture_id;
		glGenTextures(1, &texture_id);
		glBindTexture(GL_TEXTURE_2D, texture_id);
		glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_size[0], tex_size[1], 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

		this->m_postprocessor_data_textures_read.push_back(texture_id);
	}

	glGenFramebuffers(1, &this->m_postprocessor_fbo);
	glBindFramebuffer(GL_FRAMEBUFFER, this->m_postprocessor_fbo);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, this->m_postprocessor_colour_texture_write, 0);
	glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, this->m_postprocessor_depth_texture_write, 0);

	for (int i = 0; i < (int)this->m_postprocessor_data_textures_write.size(); i++)
	{
		glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1 + i, GL_TEXTURE_2D, this->m_postprocessor_data_textures_write.at(i), 0);
	}

	std::vector<GLenum> buffers;
	for (int i = 0; i < ENGINECANVAS_NUM_DATA_TEX + 1; i++)
	{
		buffers.push_back(GL_COLOR_ATTACHMENT0 + i);
	}
	glDrawBuffers(ENGINECANVAS_NUM_DATA_TEX + 1, buffers.data());

	GLenum framebuffer_status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
	if (framebuffer_status != GL_FRAMEBUFFER_COMPLETE)
	{
		throw std::runtime_error("Framebuffer error, status " + std::to_string(framebuffer_status));
	}

	glBindFramebuffer(GL_FRAMEBUFFER, 0);

	this->m_postprocessor->RegisterTexture("render_output", this->m_postprocessor_colour_texture_read, GL_TEXTURE_2D);

	if (this->m_scene != nullptr)
	{
		this->m_scene->SetReceivedOutputTextures(this->m_postprocessor_colour_texture_read, this->m_postprocessor_depth_texture_read, this->m_postprocessor_data_textures_read);
	}
}

void Renderable::SetRenderMode(RenderMode mode)
{
	this->m_rendermode = mode;
}

RenderMode Renderable::GetRenderMode()
{
	return this->m_rendermode;
}

void Renderable::SetShaderPreprocessorDefine(std::string name, std::string value)
{
	bool changes_made = false;
	for (int i = 0; i < (int)this->m_shader_defines.size(); i++)
	{
		if (std::get<0>(this->m_shader_defines.at(i)) == name)
		{
			if (std::get<1>(this->m_shader_defines.at(i)) != value)
			{
				changes_made = true;
				this->m_shader_defines.at(i) = { name, value };
			}
		}
	}

	if (changes_made)
	{
		this->RecompileShader();
	}
}
