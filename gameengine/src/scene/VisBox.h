#pragma once

#include <set>
#include <memory>

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
	std::set<VisBox*> m_pvs;
	std::set<std::shared_ptr<Model>> m_members;

public:
	VisBox();

	std::set<std::shared_ptr<Model>> GetPotentiallyVisibleModels() const;
	std::set<std::shared_ptr<Model>> GetMemberModels() const;
	void AddMemberModel(std::shared_ptr<Model> model);
	void RemoveMemberModel(std::shared_ptr<Model> model);

	void AddPotentiallyVisible(VisBox* visbox);
	void RemovePotentiallyVisible(VisBox* visbox);
};