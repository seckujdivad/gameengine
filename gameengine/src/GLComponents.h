#pragma once

#include <GL/glew.h>
#include <wx/glcanvas.h>

#include <string>

/*
Using this header (instead of including any of the headers included in this header) prevents GL/gl.h being included before GL/glew.h (which is required by GLEW).
*/

std::string GL_CHECK_ERROR();

/*
* Ideally wxWidgets should be handling all platform-specific OpenGL API calls.
* Sometimes, however, it seems to be necessary to interact with the platform-specific
* libraries. The logic for determining which library to interact with is here.
*/

#if defined(_WIN32)
	#define GAMEENGINE_USE_WGL
#elif defined(__linux__)
	#define GAMEENGINE_USE_GLX
#elif defined(__APPLE__)
	#error Apple has deprecated OpenGL. Therefore, AGL support will never be added
#else
	#error Platform unrecognised. Correct platform-specific library can't be identified
#endif