#pragma once

#include <unordered_set>

#include "OrientedBoundingBox.h"

class Model;
class VisBox;

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

class VisBox : public OrientedBoundingBox
{
private:
	std::unordered_set<VisBox*, HashPointer<VisBox>> m_pvs;
	std::unordered_set<Model*, HashPointer<Model>> m_members;

public:
	VisBox();

	std::unordered_set<Model*, HashPointer<Model>> GetPotentiallyVisibleModels() const;
	std::unordered_set<Model*, HashPointer<Model>> GetMemberModels() const;
	void AddMemberModel(Model* model);
	void RemoveMemberModel(Model* model);

	void AddPotentiallyVisible(VisBox* visbox);

};