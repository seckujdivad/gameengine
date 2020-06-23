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
#include "OrientedBoundingBox.h"
#include "../EventEmitter.h"
#include "../EventManager.h"

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

class VisBox : public Nameable, public OrientedBoundingBox, public virtual EventEmitter
{
private:
	std::unordered_set<VisBox*, HashPointer<VisBox>> m_pvs;
	std::unordered_set<Model*, HashPointer<Model>> m_members;
	
public:
	VisBox(EventManager* evtman);
	~VisBox();

	std::unordered_set<Model*, HashPointer<Model>> GetPotentiallyVisibleModels();
	std::unordered_set<Model*, HashPointer<Model>> GetMemberModels();
	void AddMemberModel(Model* model);
	void RemoveMemberModel(Model* model);
	
	void AddPotentiallyVisible(VisBox* visbox);
};