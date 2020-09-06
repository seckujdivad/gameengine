#pragma once

#include <vector>

class RenderTexture;

class CumulativeTexture
{
private:
	std::vector<RenderTexture*> m_textures;

public:
	CumulativeTexture();
	CumulativeTexture(std::vector<RenderTexture*> textures);

	void Render(int index = 0, bool continuous_draw = false) const;

	RenderTexture* GetOutput() const;
};