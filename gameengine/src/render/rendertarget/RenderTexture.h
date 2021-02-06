#pragma once

#include "../../GLComponents.h"

#include <vector>
#include <tuple>
#include <string>
#include <memory>
#include <optional>

#include "RenderTarget.h"
#include "../../scene/Referenceable.h"
#include "../texture/Texture.h"

class Engine;

class RenderTexture : public RenderTarget, public Referenceable<RenderTextureReference>
{
private:
	bool m_auto_swap_buffers;

	bool m_owns_fbo;

	std::shared_ptr<RenderTextureGroup> m_texture_write;
	std::optional<std::shared_ptr<RenderTextureGroup>> m_texture_read;

	void AttachTexturesToFramebuffer();

	void PostRenderEvent() override;

public:
	RenderTexture(RenderTextureReference reference, Engine* engine, RenderTargetConfig config, std::optional<RenderTextureGroup*> write_textures = std::optional<RenderTextureGroup*>(), bool simultaneous_read_write = false, bool auto_swap_buffers = true);
	RenderTexture(const RenderTexture&) = delete;
	RenderTexture& operator=(const RenderTexture&) = delete;
	RenderTexture(RenderTexture&&) noexcept = delete;
	RenderTexture& operator=(RenderTexture&&) noexcept = delete;
	~RenderTexture();

	std::tuple<int, int> GetOutputSize() const override;
	bool SetOutputSize(std::tuple<int, int> dimensions) override;

	void SetWriteTarget(RenderTexture* target);

	std::shared_ptr<RenderTextureGroup> GetOutputTextures() const;
	std::shared_ptr<RenderTextureGroup> GetWriteTextures() const;

	bool SwapBuffers() override;

	void SetNormalModePreviousFrameToSelf();

	void CopyFrom(const RenderTarget* src) const override;
};
