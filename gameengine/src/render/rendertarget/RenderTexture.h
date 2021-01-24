#pragma once

#include "../../GLComponents.h"

#include <vector>
#include <tuple>
#include <string>

#include "RenderTarget.h"
#include "../../scene/Referenceable.h"
#include "RenderTextureData.h"

class Engine;

class RenderTexture : public RenderTarget, public Referenceable<RenderTextureReference>
{
private:
	GLenum m_type;
	RenderTextureInfo m_info;

	bool m_simultaneous_read_write;
	bool m_auto_swap_buffers;
	RenderTextureGroup m_texture_write;
	RenderTextureGroup m_texture_read;

	std::tuple<int, int> m_dimensions;

	static void CreateTextureData(GLuint& texture, GLenum type, GLenum internal_format, GLenum format, std::tuple<int, int> dimensions, GLint filtering, bool do_create = true);

	void InitialiseTextureGroup(RenderTextureGroup& texture_group, GLenum type, bool do_create = true);
	void ResizeTextureGroup(RenderTextureGroup& texture_group);
	bool CheckTextureGroup(RenderTextureGroup texture_group) const;

	void AttachTexturesToFramebuffer();

	void PostRenderEvent() override;

public:
	RenderTexture(RenderTextureReference reference, Engine* engine, RenderTargetConfig config, RenderTextureInfo info, GLenum type = GL_TEXTURE_2D, bool simultaneous_read_write = false, bool auto_swap_buffers = true);
	~RenderTexture();

	std::tuple<int, int> GetOutputSize() const override;
	bool SetOutputSize(std::tuple<int, int> dimensions) override;

	void SetWriteTextures(RenderTextureGroup textures);
	void SetOutputTextures(RenderTextureGroup textures);

	RenderTextureGroup GetOutputTextures() const;
	RenderTextureGroup GetWriteTextures() const;
	RenderTextureInfo GetTextureInfo() const;

	bool SwapBuffers() override;

	void SetNormalModePreviousFrameToSelf();

	void CopyFrom(const RenderTarget* src) const override;
};
