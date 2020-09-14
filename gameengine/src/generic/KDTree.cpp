#include "KDTree.h"

#include <stdexcept>

template<unsigned int D, typename T, glm::qualifier Q>
KDTree<D, T, Q>::KDTree(Vector vector)
{
	this->m_point = vector;
}

template<unsigned int D, typename T, glm::qualifier Q>
KDTree<D, T, Q>::KDTree(std::vector<Vector> vectors)
{
	if (vectors.size() == 0U)
	{
		throw std::invalid_argument("You must provide at least 1 vector");
	}
	else
	{
		
	}
}

template<unsigned int D, typename T, glm::qualifier Q>
KDTree<D, T, Q>::KDTree(KDTree<D, T, Q>&& move_from) noexcept
{
	*this = std::move(move_from);
}

template<unsigned int D, typename T, glm::qualifier Q>
KDTree<D, T, Q>& KDTree<D, T, Q>::operator=(KDTree<D, T, Q>&& move_from) noexcept
{
	this->m_point = move_from.m_point;

	this->m_left = move_from.m_left;
	move_from.m_left = nullptr;

	this->m_right = move_from.m_right;
	move_from.m_right = nullptr;

	return *this;
}

template<unsigned int D, typename T, glm::qualifier Q>
KDTree<D, T, Q>::~KDTree()
{
	delete this->m_left;
	delete this->m_right;
}

template<unsigned int D, typename T, glm::qualifier Q>
void KDTree<D, T, Q>::AddPoint(Vector vector)
{

}
