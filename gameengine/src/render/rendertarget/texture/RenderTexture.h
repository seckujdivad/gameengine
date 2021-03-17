#pragma once

#include <vector>
#include <tuple>
#include <string>
#include <memory>
#include <optional>

#include "../target/RenderTarget.h"
#include "../../../scene/Referenceable.h"
#include "../../gltexture/GLTexture.h"

class Engine;
class RenderTextureGroup;

class RenderTexture : public RenderTarget, public Referenceable<RenderTextureReference>
{
private:
	bool m_auto_swap_buffers;

	bool m_owns_fbo;

	std::shared_ptr<RenderTextureGroup> m_texture_write;
	std::optional<std::shared_ptr<RenderTextureGroup>> m_texture_read;

	void PostRenderEvent() override;

public:
	RenderTexture(RenderTextureReference reference, Engine* engine, RenderTargetConfig config, std::optional<std::shared_ptr<RenderTextureGroup>> write_textures = std::optional<std::shared_ptr<RenderTextureGroup>>(), bool simultaneous_read_write = false, bool auto_swap_buffers = true);
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
	bool DoAutoSwapBuffers() const;

	void CopyFrom(const RenderTarget* src) override;
};
