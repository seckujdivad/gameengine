#include "GLComponents.h"

#include <stdexcept>

std::string GL_CHECK_ERROR()
{
	GLenum exception = glGetError();
	if (exception == GL_INVALID_ENUM)
	{
		return "GL_INVALID_ENUM";
	}
	else if (exception == GL_INVALID_VALUE)
	{
		return "GL_INVALID_VALUE";
	}
	else if (exception == GL_INVALID_OPERATION)
	{
		return "GL_INVALID_OPERATION";
	}
	else if (exception == GL_INVALID_FRAMEBUFFER_OPERATION)
	{
		return "GL_INVALID_FRAMEBUFFER_OPERATION";
	}
	else if (exception == GL_OUT_OF_MEMORY)
	{
		return "GL_OUT_OF_MEMORY";
	}
	else if (exception == GL_STACK_UNDERFLOW)
	{
		return "GL_STACK_UNDERFLOW";
	}
	else if (exception == GL_STACK_OVERFLOW)
	{
		return "GL_STACK_OVERFLOW";
	}
	else
	{
		return std::to_string(exception);
	}
}