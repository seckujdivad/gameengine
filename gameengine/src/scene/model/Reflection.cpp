#include <wx/wxprec.h>
#include "Reflection.h"

Reflection::Reflection(unsigned int texture_width, unsigned int texture_height, float near_plane, float far_plane, int refresh_frames) : Positionable(), Nameable()
{
	this->m_tex_width = texture_width;
	this->m_tex_height = texture_height;
	this->m_clip_near = near_plane;
	this->m_clip_far = far_plane;
	this->m_refresh_frames = refresh_frames;

	//make cubemap
	glGenTextures(1, &this->m_cubemap);
	glBindTexture(GL_TEXTURE_CUBE_MAP, this->m_cubemap);
	for (int i = 0; i < 6; i++)
	{
		glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, GL_RGBA, this->m_tex_width, this->m_tex_height, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
	}
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	glGenTextures(1, &this->m_cubemap_static);
	glBindTexture(GL_TEXTURE_CUBE_MAP, this->m_cubemap_static);
	for (int i = 0; i < 6; i++)
	{
		glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, GL_RGBA, this->m_tex_width, this->m_tex_height, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);
	}
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

	glBindTexture(GL_TEXTURE_CUBE_MAP, this->m_cubemap);
	glGenFramebuffers(1, &this->m_fbo);
	glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
	glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, this->m_cubemap, 0);
	//glDrawBuffer(GL_NONE);
	//glReadBuffer(GL_NONE);

	GLenum framebuffer_status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
	if (framebuffer_status != GL_FRAMEBUFFER_COMPLETE)
	{
		throw std::runtime_error("Framebuffer error, status " + std::to_string(framebuffer_status));
	}

	glBindFramebuffer(GL_FRAMEBUFFER, 0);

	this->GenerateCameraData();
}

Reflection::~Reflection()
{
	glDeleteTextures(1, &this->m_cubemap);
	glDeleteTextures(1, &this->m_cubemap_static);
	glDeleteFramebuffers(1, &this->m_fbo);
}

void Reflection::ConfigureOBB(glm::vec3 obb_position, glm::vec3 obb_dimensions, glm::vec3 obb_rotation)
{
	this->m_parallax_obb_position = obb_position;
	this->m_parallax_obb_dimensions = obb_dimensions;

	this->m_parallax_obb_rotation = glm::mat4(1.0f);
	this->m_parallax_obb_rotation = glm::rotate(this->m_parallax_obb_rotation, glm::radians(obb_rotation.x), glm::vec3(1.0f, 0.0f, 0.0f));
	this->m_parallax_obb_rotation = glm::rotate(this->m_parallax_obb_rotation, glm::radians(obb_rotation.y), glm::vec3(0.0f, 1.0f, 0.0f));
	this->m_parallax_obb_rotation = glm::rotate(this->m_parallax_obb_rotation, glm::radians(obb_rotation.z), glm::vec3(0.0f, 0.0f, 1.0f));

	this->m_parallax_obb_rotation_inverse = glm::mat4(1.0f);
	this->m_parallax_obb_rotation_inverse = glm::rotate(this->m_parallax_obb_rotation_inverse, glm::radians(0.0f - obb_rotation.z), glm::vec3(0.0f, 0.0f, 1.0f));
	this->m_parallax_obb_rotation_inverse = glm::rotate(this->m_parallax_obb_rotation_inverse, glm::radians(0.0f - obb_rotation.y), glm::vec3(0.0f, 1.0f, 0.0f));
	this->m_parallax_obb_rotation_inverse = glm::rotate(this->m_parallax_obb_rotation_inverse, glm::radians(0.0f - obb_rotation.x), glm::vec3(1.0f, 0.0f, 0.0f));
}

void Reflection::ConfigureIterative(int iterations)
{
	this->m_parallax_iterations = iterations;
}

void Reflection::InitialiseViewport()
{
	glViewport(0, 0, this->m_tex_width, this->m_tex_height);
}

void Reflection::SelectFBO(int face)
{
	glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
	glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_CUBE_MAP_POSITIVE_X + face, this->m_cubemap, 0);
}

void Reflection::CopyStaticToDynamic()
{
	glCopyImageSubData(this->m_cubemap_static, GL_TEXTURE_CUBE_MAP, 0, 0, 0, 0,
		this->m_cubemap, GL_TEXTURE_CUBE_MAP, 0, 0, 0, 0,
		this->m_tex_width, this->m_tex_height, 6);
}

void Reflection::CopyDynamicToStatic()
{
	glCopyImageSubData(this->m_cubemap, GL_TEXTURE_CUBE_MAP, 0, 0, 0, 0,
		this->m_cubemap_static, GL_TEXTURE_CUBE_MAP, 0, 0, 0, 0,
		this->m_tex_width, this->m_tex_height, 6);
}

void Reflection::RegisterUniforms(ShaderProgram* shader_program)
{
	shader_program->RegisterUniform("reflection_position");
	shader_program->RegisterTexture("reflection_cubemap", this->m_cubemap, GL_TEXTURE_CUBE_MAP);
	shader_program->RegisterUniform("reflection_isdrawing");
	shader_program->RegisterUniform("reflection_clip_near");
	shader_program->RegisterUniform("reflection_clip_far");
	shader_program->RegisterUniform("reflection_parallax_it_iterations");
	shader_program->RegisterUniform("reflection_parallax_obb_position");
	shader_program->RegisterUniform("reflection_parallax_obb_dimensions");
	shader_program->RegisterUniform("reflection_parallax_obb_rotation");
	shader_program->RegisterUniform("reflection_parallax_obb_rotation_inverse");
}

void Reflection::SetUniforms(ShaderProgram* shader_program)
{
	glm::vec3 position = glm::vec3(this->GetPosition(0),
		this->GetPosition(1), 
		this->GetPosition(2));
	glUniform3fv(shader_program->GetUniform("reflection_position"), 1, glm::value_ptr(position));
	shader_program->UpdateTexture("reflection_cubemap", this->m_cubemap);
	glUniform1f(shader_program->GetUniform("reflection_clip_near"), this->m_clip_near);
	glUniform1f(shader_program->GetUniform("reflection_clip_far"), this->m_clip_far);
	glUniform1i(shader_program->GetUniform("reflection_isdrawing"), GL_FALSE);
	glUniform1i(shader_program->GetUniform("reflection_parallax_it_iterations"), this->m_parallax_iterations);
	glUniform3fv(shader_program->GetUniform("reflection_parallax_obb_position"), 1, glm::value_ptr(this->m_parallax_obb_position));
	glUniform3fv(shader_program->GetUniform("reflection_parallax_obb_dimensions"), 1, glm::value_ptr(this->m_parallax_obb_dimensions));
	glUniformMatrix3fv(shader_program->GetUniform("reflection_parallax_obb_rotation"), 1, GL_FALSE, glm::value_ptr(glm::mat3(this->m_parallax_obb_rotation)));
	glUniformMatrix3fv(shader_program->GetUniform("reflection_parallax_obb_rotation_inverse"), 1, GL_FALSE, glm::value_ptr(glm::mat3(this->m_parallax_obb_rotation_inverse)));

}

void Reflection::GenerateCameraData()
{
	this->m_transform_translate = glm::vec4(0 - this->GetPosition(0),
		0 - this->GetPosition(1),
		0 - this->GetPosition(2),
		0.0f);

	this->m_transform_perspective = glm::perspective(glm::pi<float>() / 2.0f, 1.0f, this->m_clip_near, this->m_clip_far);

	glm::vec3 translate = glm::vec3(0.0f, 0.0f, 0.0f);
	this->m_transform_rotate.push_back(glm::lookAt(translate, translate + glm::vec3(1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
	this->m_transform_rotate.push_back(glm::lookAt(translate, translate + glm::vec3(-1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
	this->m_transform_rotate.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f)));
	this->m_transform_rotate.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, -1.0f, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f)));
	this->m_transform_rotate.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, 0.0f, 1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
	this->m_transform_rotate.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, 0.0f, -1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
}

void Reflection::AddStaticModel(std::string identifier)
{
	if (!this->ModelIsStatic(identifier))
	{
		this->m_models_static.push_back(identifier);
	}
}

void Reflection::RemoveStaticModel(std::string identifier)
{
	std::vector<int> to_remove;

	for (size_t i = 0; i < this->m_models_static.size(); i++)
	{
		if (this->m_models_static.at(i) == identifier)
		{
			to_remove.push_back(i);
		}
	}

	for (int i = to_remove.size() - 1; i > -1; i--)
	{
		this->m_models_static.erase(this->m_models_static.begin() + to_remove.at(i));
	}
}

bool Reflection::ModelIsStatic(std::string identifier)
{
	for (size_t i = 0; i < this->m_models_static.size(); i++)
	{
		if (this->m_models_static.at(i) == identifier)
		{
			return true;
		}
	}
	return false;
}

void Reflection::AddDynamicModel(std::string identifier)
{
	if (!this->ModelIsDynamic(identifier))
	{
		this->m_models_dynamic.push_back(identifier);
	}
}

void Reflection::RemoveDynamicModel(std::string identifier)
{
	std::vector<int> to_remove;

	for (size_t i = 0; i < this->m_models_dynamic.size(); i++)
	{
		if (this->m_models_dynamic.at(i) == identifier)
		{
			to_remove.push_back(i);
		}
	}

	for (int i = to_remove.size() - 1; i > -1; i--)
	{
		this->m_models_dynamic.erase(this->m_models_dynamic.begin() + to_remove.at(i));
	}
}

bool Reflection::ModelIsDynamic(std::string identifier)
{
	for (size_t i = 0; i < this->m_models_dynamic.size(); i++)
	{
		if (this->m_models_dynamic.at(i) == identifier)
		{
			return true;
		}
	}
	return false;
}

void Reflection::RegisterGenerateUniforms(ShaderProgram* shader_program)
{
	//no need to register uniforms as the camera will register those for us
}

void Reflection::SetGenerateUniforms(ShaderProgram* shader_program, int face)
{
	glUniform4fv(shader_program->GetUniform("cam_translate"), 1, glm::value_ptr(this->m_transform_translate));
	glUniformMatrix4fv(shader_program->GetUniform("cam_rotate"), 1, GL_FALSE, glm::value_ptr(this->m_transform_rotate.at(face)));
	glUniformMatrix4fv(shader_program->GetUniform("cam_persp"), 1, GL_FALSE, glm::value_ptr(this->m_transform_perspective));
	glUniform1i(shader_program->GetUniform("reflection_isdrawing"), GL_TRUE);
	glUniform1f(shader_program->GetUniform("cam_clip_near"), this->m_clip_near);
	glUniform1f(shader_program->GetUniform("cam_clip_far"), this->m_clip_far);
}

void Reflection::IncrementFrameCounter(int increment)
{
	this->m_frames_since_last_refresh += increment;
}

bool Reflection::DynamicNeedsRedrawing(bool reset_if_redraw)
{
	if (this->m_refresh_frames < 0)
	{
		this->m_frames_since_last_refresh = 0;
		return false;
	}
	else
	{
		bool result = this->m_frames_since_last_refresh >= this->m_refresh_frames;

		if (result && reset_if_redraw)
		{
			this->m_frames_since_last_refresh = 0;
		}

		return result;
	}
}