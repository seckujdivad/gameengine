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

	this->UpdateCameraData();
}

Reflection::~Reflection()
{
	glDeleteTextures(1, &this->m_cubemap);
	glDeleteTextures(1, &this->m_cubemap_static);
	glDeleteFramebuffers(1, &this->m_fbo);
}

void Reflection::SetNearClip(float near_clip)
{
}

float Reflection::GetNearClip()
{
	return 0.0f;
}

void Reflection::SetFarClip(float far_clip)
{
}

float Reflection::GetFarClip()
{
	return 0.0f;
}

void Reflection::ConfigureIterative(int iterations)
{
	this->m_parallax_iterations = iterations;
}

void Reflection::AddOBB(OrientedBoundingBox obb)
{
	this->m_parallax_obbs.push_back(obb);
}

int Reflection::GetOBBCount()
{
	return (int)this->m_parallax_obbs.size();
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

void Reflection::RegisterUniforms(ShaderProgram* shader_program, int index)
{
	shader_program->RegisterUniform("reflection_isdrawing");

	shader_program->RegisterTexture("reflection_cubemaps[" + std::to_string(index) + "]", this->m_cubemap, GL_TEXTURE_CUBE_MAP);

	std::string prefix1 = "reflections[" + std::to_string(index) + "].";
	std::string prefix2;

	shader_program->RegisterUniform(prefix1 + "position");
	shader_program->RegisterUniform(prefix1 + "clip_near");
	shader_program->RegisterUniform(prefix1 + "clip_far");
	shader_program->RegisterUniform(prefix1 + "iterations");
	shader_program->RegisterUniform(prefix1 + "num_obbs");
	shader_program->RegisterUniform(prefix1 + "mode");
	
	for (int i = 0; i < (int)this->m_parallax_obbs.size(); i++)
	{
		prefix2 = prefix1 + "parallax_obbs[" + std::to_string(i) + "].";

		shader_program->RegisterUniform(prefix2 + "position");
		shader_program->RegisterUniform(prefix2 + "dimensions");
		shader_program->RegisterUniform(prefix2 + "rotation");
		shader_program->RegisterUniform(prefix2 + "rotation_inverse");
	}
}

void Reflection::SetUniforms(ShaderProgram* shader_program, int index, int mode)
{
	glUniform1i(shader_program->GetUniform("reflection_isdrawing"), GL_FALSE);

	shader_program->UpdateTexture("reflection_cubemaps[" + std::to_string(index) + "]", this->m_cubemap);

	std::string prefix1 = "reflections[" + std::to_string(index) + "].";
	std::string prefix2;
	glUniform3fv(shader_program->GetUniform(prefix1 + "position"), 1, glm::value_ptr(this->GetPositionVec()));
	glUniform1f(shader_program->GetUniform(prefix1 + "clip_near"), this->m_clip_near);
	glUniform1f(shader_program->GetUniform(prefix1 + "clip_far"), this->m_clip_far);
	glUniform1i(shader_program->GetUniform(prefix1 + "iterations"), this->m_parallax_iterations);
	glUniform1i(shader_program->GetUniform(prefix1 + "num_obbs"), (int)this->m_parallax_obbs.size());
	glUniform1i(shader_program->GetUniform(prefix1 + "mode"), mode);

	for (int i = 0; i < (int)this->m_parallax_obbs.size(); i++)
	{
		prefix2 = prefix1 + "parallax_obbs[" + std::to_string(i) + "].";

		glUniform3fv(shader_program->GetUniform(prefix2 + "position"), 1, glm::value_ptr(this->m_parallax_obbs.at(i).GetPositionVec()));
		glUniform3fv(shader_program->GetUniform(prefix2 + "dimensions"), 1, glm::value_ptr(this->m_parallax_obbs.at(i).GetDimensionsVec()));
		glUniformMatrix3fv(shader_program->GetUniform(prefix2 + "rotation"), 1, GL_FALSE, glm::value_ptr(this->m_parallax_obbs.at(i).GetRotationMatrix()));
		glUniformMatrix3fv(shader_program->GetUniform(prefix2 + "rotation_inverse"), 1, GL_FALSE, glm::value_ptr(this->m_parallax_obbs.at(i).GetInverseRotationMatrix()));
	}
}

void Reflection::UpdateCameraData()
{
	bool remake_composite = false;

	if (this->CheckIfRepositioned(true))
	{
		this->m_transform_translate = glm::vec4(0 - this->GetPosition(0),
			0 - this->GetPosition(1),
			0 - this->GetPosition(2),
			0.0f);
		
		remake_composite = true;
	}

	if (this->m_clips_changed)
	{
		this->m_transform_perspective = glm::perspective(glm::pi<float>() / 2.0f, 1.0f, this->m_clip_near, this->m_clip_far);

		this->m_clips_changed = false;
		remake_composite = true;
	}

	if (this->m_transform_rotate.size() == 0)
	{
		glm::vec3 translate = glm::vec3(0.0f, 0.0f, 0.0f);
		this->m_transform_rotate.push_back(glm::lookAt(translate, translate + glm::vec3(1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
		this->m_transform_rotate.push_back(glm::lookAt(translate, translate + glm::vec3(-1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
		this->m_transform_rotate.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f)));
		this->m_transform_rotate.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, -1.0f, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f)));
		this->m_transform_rotate.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, 0.0f, 1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
		this->m_transform_rotate.push_back(glm::lookAt(translate, translate + glm::vec3(0.0f, 0.0f, -1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));

		remake_composite = true;
	}

	if (remake_composite || (this->m_transform_combined.size() == 0))
	{
		this->m_transform_combined.clear();
		this->m_transform_inverse_combined.clear();

		for (int i = 0; i < 6; i++)
		{
			glm::mat4 matrix = glm::mat4(1.0f);
			matrix = glm::translate(matrix, glm::vec3(this->m_transform_translate));
			matrix = this->m_transform_perspective * this->m_transform_rotate.at(i) * matrix;
			this->m_transform_combined.push_back(matrix);
			this->m_transform_inverse_combined.push_back(glm::inverse(matrix));
		}
	}
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

	for (int i = 0; i < (int)this->m_models_static.size(); i++)
	{
		if (this->m_models_static.at(i) == identifier)
		{
			to_remove.push_back(i);
		}
	}

	for (int i = (int)to_remove.size() - 1; i > -1; i--)
	{
		this->m_models_static.erase(this->m_models_static.begin() + to_remove.at(i));
	}
}

bool Reflection::ModelIsStatic(std::string identifier)
{
	for (int i = 0; i < (int)this->m_models_static.size(); i++)
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

	for (int i = 0; i < (int)this->m_models_dynamic.size(); i++)
	{
		if (this->m_models_dynamic.at(i) == identifier)
		{
			to_remove.push_back(i);
		}
	}

	for (int i = (int)to_remove.size() - 1; i > -1; i--)
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
	this->UpdateCameraData();

	glUniform4fv(shader_program->GetUniform("cam_translate"), 1, glm::value_ptr(this->m_transform_translate));
	glUniformMatrix4fv(shader_program->GetUniform("cam_rotate"), 1, GL_FALSE, glm::value_ptr(this->m_transform_rotate.at(face)));
	glUniformMatrix4fv(shader_program->GetUniform("cam_persp"), 1, GL_FALSE, glm::value_ptr(this->m_transform_perspective));
	glUniform1i(shader_program->GetUniform("reflection_isdrawing"), GL_TRUE);
	glUniform1f(shader_program->GetUniform("cam_clip_near"), this->m_clip_near);
	glUniform1f(shader_program->GetUniform("cam_clip_far"), this->m_clip_far);
	glUniformMatrix4fv(shader_program->GetUniform("cam_transform"), 1, GL_FALSE, glm::value_ptr(this->m_transform_combined.at(face)));
	glUniformMatrix4fv(shader_program->GetUniform("cam_transform_inverse"), 1, GL_FALSE, glm::value_ptr(this->m_transform_inverse_combined.at(face)));
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