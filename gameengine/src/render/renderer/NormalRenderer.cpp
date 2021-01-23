#include "NormalRenderer.h"

#include <stdexcept>

#include "../rendertarget/RenderTarget.h"

NormalRenderer::NormalRenderer(Engine* engine, RenderTarget* target) : Renderer(engine, target)
{
}

bool NormalRenderer::SetOutputSize(std::tuple<int, int> dimensions)
{
	return this->GetTarget()->SetOutputSize(dimensions);
}

void NormalRenderer::CopyFrom(const Renderer* src) const
{
	if (src != this)
	{
		this->GetTarget()->CopyFrom(src->GetTarget());
	}
}

std::unordered_set<RenderTextureReference> NormalRenderer::GetRenderTextureDependencies() const
{
	return this->GetTarget()->GetRenderTextureDependencies();
}

void NormalRenderer::Render(std::vector<Model*> models, bool continuous_draw)
{
	this->GetTarget()->Render(models, continuous_draw);
}
