#pragma once

#include <GL/glew.h>
#include <wx/glcanvas.h>

/*
Using this header (instead of including any of the headers included in this header) prevents GL/gl.h being included before GL/glew.h (which is required by GLEW).
*/