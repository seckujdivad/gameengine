#include "WrapperRenderer.h"

#include <stdexcept>

#include "../rendertarget/RenderTarget.h"

WrapperRenderer::WrapperRenderer(Engine* engine, RenderTarget* target) : Renderer(engine, target)
{
}

bool WrapperRenderer::SetOutputSize(std::tuple<int, int> dimensions)
{
	return this->GetTarget()->SetOutputSize(dimensions);
}

void WrapperRenderer::CopyFrom(const Renderer* src) const
{
	if (src != this)
	{
		this->GetTarget()->CopyFrom(src->GetTarget());
	}
}

std::unordered_set<RenderTextureReference> WrapperRenderer::GetRenderTextureDependencies() const
{
	return this->GetTarget()->GetRenderTextureDependencies();
}

void WrapperRenderer::Render(std::vector<Model*> models, bool continuous_draw)
{
	this->GetTarget()->Render(models, continuous_draw);
}