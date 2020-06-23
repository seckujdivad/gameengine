#include <wx/wxprec.h>
#include "LocallyMovable.h"

LocallyMovable::LocallyMovable(EventManager* evtman) : Positionable(evtman), Rotatable(evtman), EventEmitter(evtman)
{

}

void LocallyMovable::MoveLocally(float x, float y, float z)
{

	glm::vec4 translation = glm::vec4(0.0f - x, 0.0f - y, 0.0f - z, 1.0f);

	glm::mat4 rotation = glm::mat4(1.0f);
	glm::vec3 axis;
	for (int i = 2; i > -1; i--)
	{
		axis = glm::vec4(0.0f);
		axis[i] = 1.0f;
		rotation = glm::rotate(rotation, glm::radians(this->GetRotation(i)), axis);
	}

	translation = rotation * translation;

	glm::vec3 final_translation = glm::vec3(translation);

	for (int i = 0; i < 3; i++)
	{
		this->SetPosition(i, this->GetPosition(i) + final_translation[i]);
	}

}