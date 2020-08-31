#include "GLComponents.h"

void GL_CHECK_ERROR()
{
	GLenum exception = glGetError();
	if (exception == GL_INVALID_ENUM)
	{
		throw std::runtime_error("GL_INVALID_ENUM");
	}
	else if (exception == GL_INVALID_VALUE)
	{
		throw std::runtime_error("GL_INVALID_VALUE");
	}
	else if (exception == GL_INVALID_OPERATION)
	{
		throw std::runtime_error("GL_INVALID_OPERATION");
	}
	else if (exception == GL_INVALID_FRAMEBUFFER_OPERATION)
	{
		throw std::runtime_error("GL_INVALID_FRAMEBUFFER_OPERATION");
	}
	else if (exception == GL_OUT_OF_MEMORY)
	{
		throw std::runtime_error("GL_OUT_OF_MEMORY");
	}
	else if (exception == GL_STACK_UNDERFLOW)
	{
		throw std::runtime_error("GL_STACK_UNDERFLOW");
	}
	else if (exception == GL_STACK_OVERFLOW)
	{
		throw std::runtime_error("GL_STACK_OVERFLOW");
	}
}