#include "GLFramebuffer.h"

GLFramebuffer::GLFramebuffer(GLuint fbo, bool is_owning) : GLObjectLabelable(GL_FRAMEBUFFER), m_fbo(fbo), m_is_owning(is_owning)
{
	this->SetName(fbo);
}

GLFramebuffer::GLFramebuffer(GLFramebuffer&& move_from) noexcept : GLObjectLabelable(GL_FRAMEBUFFER)
{
	*this = std::move(move_from);
}

GLFramebuffer& GLFramebuffer::operator=(GLFramebuffer&& move_from) noexcept
{
	this->m_fbo = move_from.m_fbo;
	this->m_is_owning = move_from.m_is_owning;

	this->SetName(this->m_fbo);

	move_from.m_is_owning = false;

	return *this;
}

GLFramebuffer::~GLFramebuffer()
{
	if (this->m_is_owning)
	{
		glDeleteFramebuffers(1, &this->m_fbo);
	}
}

GLuint GLFramebuffer::GetFBO() const
{
	return this->m_fbo;
}

bool GLFramebuffer::IsValidFBO() const
{
	return (this->m_fbo == 0) || glIsFramebuffer(this->m_fbo);
}

bool GLFramebuffer::OwnsFBO() const
{
	return this->m_is_owning;
}

void GLFramebuffer::Bind() const
{
	glBindFramebuffer(GL_FRAMEBUFFER, this->m_fbo);
}

bool GLFramebuffer::operator==(const GLFramebuffer& compare) const
{
	return this->m_fbo == compare.m_fbo;
}

bool GLFramebuffer::operator!=(const GLFramebuffer& compare) const
{
	return !(*this == compare);
}
