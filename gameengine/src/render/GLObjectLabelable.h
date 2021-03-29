#pragma once

#include <string>
#include <optional>

#include "../GLComponents.h"

class GLObjectLabelable
{
private:
	GLenum m_identifier;
	std::optional<GLuint> m_name;

	std::optional<std::string> m_label;

	void PushLabel() const;

protected:
	void SetName(GLuint name);

public:
	GLObjectLabelable(GLenum identifier);

	void SetLabel(std::string label);
	std::optional<std::string> GetLabel() const;
};