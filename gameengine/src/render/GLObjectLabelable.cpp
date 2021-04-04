#include "GLObjectLabelable.h"

void GLObjectLabelable::PushLabel() const
{
	if (this->m_label.has_value() && this->m_name.has_value())
	{
		glObjectLabel(this->m_identifier, this->m_name.value(), static_cast<GLsizei>(this->m_label->size()), this->m_label->c_str());
	}
}

void GLObjectLabelable::SetName(GLuint name)
{
	this->m_name = name;
	this->PushLabel();
}

void GLObjectLabelable::SetLabel(std::string label)
{
	this->m_label = label;
	this->PushLabel();
}

GLObjectLabelable::GLObjectLabelable(GLenum identifier) : m_identifier(identifier)
{
}

std::optional<std::string> GLObjectLabelable::GetLabel() const
{
	return this->m_label;
}
