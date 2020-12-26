#pragma once

class Scene;

class SceneChild
{
private:
	Scene* m_scene = nullptr;

public:
	SceneChild(Scene* scene);
	virtual ~SceneChild();

	Scene* GetScene() const;

	static bool IsChildOfSameScene(SceneChild* first, SceneChild* second);
	bool IsChildOfSameScene(SceneChild* other);
};