#pragma once

#include <glm/glm.hpp>
#include <glm/ext.hpp>

class Scalable
{
private:
	glm::dvec3 m_scale = glm::dvec3(1.0);

	bool m_rescaled = true;
	
public:
	Scalable();

	void SetScale(double x, double y, double z);
	void SetScale(int index, double value);
	void SetScale(glm::dvec3 scale);

	double GetScale(int index) const;
	glm::dvec3 GetScale() const;

	glm::dmat4 GetScaleMatrix() const;
	glm::dmat4 GetScaleMatrixInverse() const;

	bool CheckIfRescaled(bool reset = true);
};