#include "WrapperRenderer.h"

#include <stdexcept>

#include "../rendertarget/target/RenderTarget.h"

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

void WrapperRenderer::Render(std::vector<Model*> models, std::clock_t draw_time, bool continuous_draw)
{
	this->GetTarget()->Render(models, draw_time, continuous_draw);
}
