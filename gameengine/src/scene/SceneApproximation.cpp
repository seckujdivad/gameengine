#include "SceneApproximation.h"

SceneApproximation::SceneApproximation()
{
}

void SceneApproximation::AddOBB(OrientedBoundingBox obb)
{
	this->m_obbs.push_back(obb);
}

void SceneApproximation::RegisterUniforms(ShaderProgram* shader_program)
{
	std::string prefix;

	for (int i = 0; i < (int)this->m_obbs.size(); i++)
	{
		prefix = "scene_approximations[" + std::to_string(i) + "].";
		shader_program->RegisterUniform(prefix + "position");
		shader_program->RegisterUniform(prefix + "dimensions");
		shader_program->RegisterUniform(prefix + "rotation");
		shader_program->RegisterUniform(prefix + "rotation_inverse");
	}
}

void SceneApproximation::SetUniforms(ShaderProgram* shader_program)
{
	std::string prefix;

	for (int i = 0; i < (int)this->m_obbs.size(); i++)
	{
		prefix = "scene_approximations[" + std::to_string(i) + "].";
		glUniform3fv(shader_program->GetUniform(prefix + "position"), 1, glm::value_ptr(this->m_obbs.at(i).GetPositionVec()));
		glUniform3fv(shader_program->GetUniform(prefix + "dimensions"), 1, glm::value_ptr(this->m_obbs.at(i).GetDimensionsVec()));
		glUniformMatrix3fv(shader_program->GetUniform(prefix + "rotation"), 1, GL_FALSE, glm::value_ptr(this->m_obbs.at(i).GetRotationMatrix()));
		glUniformMatrix3fv(shader_program->GetUniform(prefix + "rotation_inverse"), 1, GL_FALSE, glm::value_ptr(this->m_obbs.at(i).GetInverseRotationMatrix()));
	}
}

int SceneApproximation::NumOBBs()
{
	return (int)this->m_obbs.size();
}
