#include <wx/wxprec.h>
#include "VisBox.h"

VisBox::VisBox() : Nameable(), Rotatable(), Positionable(), Scalable()
{
}

VisBox::~VisBox()
{
}

void VisBox::SetOBBBias(glm::vec3 obb_bias)
{
	this->m_obb_bias = obb_bias;
}

glm::vec3 VisBox::GetOBBBias()
{
	return this->m_obb_bias;
}

std::unordered_set<Model*, HashPointer<Model>> VisBox::GetPotentiallyVisibleModels()
{
	std::unordered_set<Model*, HashPointer<Model>> output = this->m_members;

	for (auto it = this->m_pvs.begin(); it != this->m_pvs.end(); it++)
	{
		VisBox* visbox = *it;
		std::unordered_set<Model*, HashPointer<Model>> member_models = visbox->GetMemberModels();
		output.insert(member_models.begin(), member_models.end());
	}
	
	return output;
}

std::unordered_set<Model*, HashPointer<Model>> VisBox::GetMemberModels()
{
	return this->m_members;
}

void VisBox::AddMemberModel(Model* model)
{
	this->m_members.insert(model);
}

void VisBox::RemoveMemberModel(Model* model)
{
	this->m_members.erase(this->m_members.find(model));
}

void VisBox::AddPotentiallyVisible(VisBox* visbox)
{
	this->m_pvs.insert(visbox);
}

bool VisBox::PointInOBB(glm::vec3 point)
{
	//set up matrices
	glm::mat4 rotation_4d = glm::mat4(1.0f);
	rotation_4d = glm::rotate(rotation_4d, glm::radians(this->GetRotation(0)), glm::vec3(1.0f, 0.0f, 0.0f));
	rotation_4d = glm::rotate(rotation_4d, glm::radians(this->GetRotation(1)), glm::vec3(0.0f, 1.0f, 0.0f));
	rotation_4d = glm::rotate(rotation_4d, glm::radians(this->GetRotation(2)), glm::vec3(0.0f, 0.0f, 1.0f));
	glm::mat3 rotation = glm::mat3(rotation_4d);

	glm::mat4 rotation_inverse_4d = glm::mat4(1.0f);
	rotation_inverse_4d = glm::rotate(rotation_inverse_4d, glm::radians(0 - this->GetRotation(0)), glm::vec3(1.0f, 0.0f, 0.0f));
	rotation_inverse_4d = glm::rotate(rotation_inverse_4d, glm::radians(0 - this->GetRotation(1)), glm::vec3(0.0f, 1.0f, 0.0f));
	rotation_inverse_4d = glm::rotate(rotation_inverse_4d, glm::radians(0 - this->GetRotation(2)), glm::vec3(0.0f, 0.0f, 1.0f));
	glm::mat3 rotation_inverse = glm::mat3(rotation_inverse_4d);

	glm::vec3 position = glm::vec3(this->GetPosition(0),
		this->GetPosition(1),
		this->GetPosition(2));
	
	glm::vec3 dimensions = glm::vec3(this->GetScale(0) * 2.0f,
		this->GetScale(1) * 2.0f,
		this->GetScale(2) * 2.0f);
	
	glm::vec3 translation = position - (0.5f * rotation * dimensions);

	glm::vec3 obb_space_point = rotation_inverse * (point - translation);

	for (int i = 0; i < 3; i++)
	{
		if ((0.0f - this->m_obb_bias[i] > obb_space_point[i]) || (dimensions[i] + this->m_obb_bias[i] < obb_space_point[i]))
		{
			return false;
		}
	}

	return true;
}

bool VisBox::PointInOBB(float x, float y, float z)
{
	return this->PointInOBB(glm::vec3(x, y, z));
}
