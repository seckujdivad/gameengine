#pragma once

#include <glm/glm.hpp>
#include <glm/ext.hpp>

#include <vector>
#include <type_traits>

/*
* Unfinished k-d tree implementation
* It was intended to be used to implement the MergeVertices method, but a much simpler method was found
* Until I can find a potential use for it, I will leave it here unfinished
*/
template<unsigned int D, typename T, glm::qualifier Q>
class KDTree
{
public:
	using Vector = glm::vec<D, T, Q>;

	static_assert(D > 0, "Dimensions must be greater than 0");
	static_assert(D < 5, "Dimensions must be less than 5");

private:
	Vector m_point;
	
	KDTree* m_left = nullptr;
	KDTree* m_right = nullptr;

public:
	KDTree(Vector vector);
	KDTree(std::vector<Vector> vectors);
	KDTree(const KDTree&) = delete;
	KDTree& operator=(const KDTree&) = delete;
	KDTree(KDTree<D, T, Q>&& move_from) noexcept;
	KDTree<D, T, Q>& operator=(KDTree<D, T, Q>&& move_from) noexcept;
	~KDTree();

	void AddPoint(Vector vector);
};

//glm::dvecX
template<> class KDTree<1, glm::f64, glm::defaultp>;
template<> class KDTree<2, glm::f64, glm::defaultp>;
template<> class KDTree<3, glm::f64, glm::defaultp>;
template<> class KDTree<4, glm::f64, glm::defaultp>;


//glm::vecX
template<> class KDTree<1, glm::f32, glm::defaultp>;
template<> class KDTree<2, glm::f32, glm::defaultp>;
template<> class KDTree<3, glm::f32, glm::defaultp>;
template<> class KDTree<4, glm::f32, glm::defaultp>;