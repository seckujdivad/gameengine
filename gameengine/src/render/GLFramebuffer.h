#pragma once

#include "../GLComponents.h"

#include "GLObjectLabelable.h"

class GLFramebuffer : public GLObjectLabelable
{
private:
	GLuint m_fbo;
	bool m_is_owning;

public:
	GLFramebuffer(GLuint fbo, bool is_owning);
	GLFramebuffer(const GLFramebuffer&) = delete;
	GLFramebuffer& operator=(const GLFramebuffer&) = delete;
	GLFramebuffer(GLFramebuffer&& move_from) noexcept;
	GLFramebuffer& operator=(GLFramebuffer&& move_from) noexcept;
	~GLFramebuffer();

	GLuint GetFBO() const;
	bool IsValidFBO() const;
	bool OwnsFBO() const;

	void Bind() const;

	bool operator==(const GLFramebuffer& compare) const;
	bool operator!=(const GLFramebuffer& compare) const;
};