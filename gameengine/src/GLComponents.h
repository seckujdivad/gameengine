#pragma once

#include <GL/glew.h>
#include <wx/glcanvas.h>

#include <stdexcept>

/*
Using this header (instead of including any of the headers included in this header) prevents GL/gl.h being included before GL/glew.h (which is required by GLEW).
*/

void GL_CHECK_ERROR();