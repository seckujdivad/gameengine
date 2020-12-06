#include "std_glm.h"

#include <stdexcept>
#include <string>

template<glm::length_t L, typename T>
glm::vec<L, T> GetGLMVector(std::vector<T> vector)
{
	static_assert(L > 0, "L must be greater than 0");
	static_assert(L <= 4, "L must be less than or equal to 4");

	if (std::size_t(L) == vector.size())
	{
		glm::vec<L, T> result = glm::vec<L, T>(static_cast<T>(0));
		for (size_t i = 0; i < vector.size(); i++)
		{
			result[glm::length_t(i)] = vector.at(i);
		}
		return result;
	}
	else
	{
		throw std::invalid_argument("\"vector\" must have size " + std::to_string(L) + ", not " + std::to_string(vector.size()));
	}
}

template glm::vec<1, float> GetGLMVector(std::vector<float>);
template glm::vec<2, float> GetGLMVector(std::vector<float>);
template glm::vec<3, float> GetGLMVector(std::vector<float>);
template glm::vec<4, float> GetGLMVector(std::vector<float>);

template glm::vec<1, double> GetGLMVector(std::vector<double>);
template glm::vec<2, double> GetGLMVector(std::vector<double>);
template glm::vec<3, double> GetGLMVector(std::vector<double>);
template glm::vec<4, double> GetGLMVector(std::vector<double>);

template glm::vec<1, int> GetGLMVector(std::vector<int>);
template glm::vec<2, int> GetGLMVector(std::vector<int>);
template glm::vec<3, int> GetGLMVector(std::vector<int>);
template glm::vec<4, int> GetGLMVector(std::vector<int>);

template glm::vec<1, bool> GetGLMVector(std::vector<bool>);
template glm::vec<2, bool> GetGLMVector(std::vector<bool>);
template glm::vec<3, bool> GetGLMVector(std::vector<bool>);
template glm::vec<4, bool> GetGLMVector(std::vector<bool>);

template<glm::length_t L, typename T>
std::vector<T> GetSTDVector(glm::vec<L, T> vector)
{
	static_assert(L > 0, "L must be greater than 0");
	static_assert(L <= 4, "L must be less than or equal to 4");

	std::vector<T> result;
	for (glm::length_t i = 0; i < L; i++)
	{
		result.push_back(vector[i]);
	}
	return result;
}

template std::vector<float> GetSTDVector(glm::vec<1, float>);
template std::vector<float> GetSTDVector(glm::vec<2, float>);
template std::vector<float> GetSTDVector(glm::vec<3, float>);
template std::vector<float> GetSTDVector(glm::vec<4, float>);

template std::vector<double> GetSTDVector(glm::vec<1, double>);
template std::vector<double> GetSTDVector(glm::vec<2, double>);
template std::vector<double> GetSTDVector(glm::vec<3, double>);
template std::vector<double> GetSTDVector(glm::vec<4, double>);

template std::vector<int> GetSTDVector(glm::vec<1, int>);
template std::vector<int> GetSTDVector(glm::vec<2, int>);
template std::vector<int> GetSTDVector(glm::vec<3, int>);
template std::vector<int> GetSTDVector(glm::vec<4, int>);

template std::vector<bool> GetSTDVector(glm::vec<1, bool>);
template std::vector<bool> GetSTDVector(glm::vec<2, bool>);
template std::vector<bool> GetSTDVector(glm::vec<3, bool>);
template std::vector<bool> GetSTDVector(glm::vec<4, bool>);