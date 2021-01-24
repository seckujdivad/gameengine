#include "Renderer.h"

#include "../rendertarget/RenderTarget.h"

Renderer::Renderer(Engine* engine, RenderTarget* target) : m_engine(engine), m_target(target)
{
}

Renderer::~Renderer()
{
}

Engine* Renderer::GetEngine() const
{
	return this->m_engine;
}

RenderTarget* Renderer::GetTarget() const
{
	return this->m_target;
}

std::tuple<int, int> Renderer::GetOutputSize() const
{
	return this->m_target->GetOutputSize();
}

void Renderer::CopyTo(const Renderer* dest) const
{
	dest->CopyFrom(this);
}

void Renderer::SetCamera(Camera* camera)
{
	this->GetTarget()->SetCamera(camera);
}

Camera* Renderer::GetCamera() const
{
	return this->GetTarget()->GetCamera();
}
