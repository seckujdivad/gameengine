#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <unordered_set>

#include "Nameable.h"
#include "Positionable.h"
#include "Rotatable.h"
#include "Scalable.h"
#include "model/Model.h"

template <typename T>
class HashPointer : public std::hash<T*>
{
public:
	size_t operator()(const T* to_hash) const // Thomas Wang's hash function https://jfdube.wordpress.com/2011/10/12/hashing-strings-and-pointers-avoiding-common-pitfalls/
	{
		size_t Value = (size_t)to_hash;
		Value = ~Value + (Value << 15);
		Value = Value ^ (Value >> 12);
		Value = Value + (Value << 2);
		Value = Value ^ (Value >> 4);
		Value = Value * 2057;
		Value = Value ^ (Value >> 16);
		return Value;
	}
};

class VisBox : public Nameable, public Positionable, public Rotatable, public Scalable
{
private:
	glm::vec3 m_obb_bias = glm::vec3(0.0f, 0.0f, 0.0f);

	std::unordered_set<VisBox*, HashPointer<VisBox>> m_pvs;
	std::unordered_set<Model*, HashPointer<Model>> m_members;
	
public:
	VisBox();
	~VisBox();

	void SetOBBBias(glm::vec3 obb_bias);
	glm::vec3 GetOBBBias();

	std::unordered_set<Model*, HashPointer<Model>> GetPotentiallyVisibleModels();
	std::unordered_set<Model*, HashPointer<Model>> GetMemberModels();
	void AddMemberModel(Model* model);
	void RemoveMemberModel(Model* model);
	
	void AddPotentiallyVisible(VisBox* visbox);

	bool PointInOBB(glm::vec3 point);
	bool PointInOBB(float x, float y, float z);
};