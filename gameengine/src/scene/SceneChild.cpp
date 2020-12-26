#include "SceneChild.h"

SceneChild::SceneChild(Scene* scene) : m_scene(scene)
{
}

SceneChild::~SceneChild()
{
}

Scene* SceneChild::GetScene() const
{
	return this->m_scene;
}

bool SceneChild::IsChildOfSameScene(SceneChild* first, SceneChild* second)
{
	Scene* first_scene = first->GetScene();
	Scene* second_scene = second->GetScene();

	if ((first_scene == nullptr) || (second_scene == nullptr))
	{
		return false;
	}

	return first_scene == second_scene;
}

bool SceneChild::IsChildOfSameScene(SceneChild* other)
{
	return SceneChild::IsChildOfSameScene(this, other);
}
