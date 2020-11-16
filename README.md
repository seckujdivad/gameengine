OpenGL renderer using wxWidgets components.

[![Actions Status](https://github.com/seckujdivad/gameengine/workflows/Windows%20build%20and%20test/badge.svg)](https://github.com/seckujdivad/gameengine/actions)

# Build Dependencies
## vcpkg
Build dependencies are handled by [vcpkg](https://github.com/microsoft/vcpkg). You must set up vcpkg and install the required libraries:
* nlohmann-json
* wxwidgets
* glm
* glew
* opengl
* gtest

You can also use the response file at `/vcpkg_install.txt`.

This project statically links to wxWidgets. vcpkg builds the dynamic version by default, so you will need to edit the CMake files for your platform triplets. For 64-bit Windows, the triplet is `x64-windows`. Therefore, you will need to edit the file at `vcpkg/triplets/x64-windows.cmake` before building and add the following to the end before installing wxWidgets through vcpkg:
```cmake
if (PORT STREQUAL wxwidgets)
	set(VCPKG_LIBRARY_LINKAGE static)
endif()
```

## Python
When the project is built in `Release` mode, Visual Studio runs a Python script that creates ZIP archives containing all the runtime dependencies for the x86 and x64 versions of the application. This script should run on any Python 3 version without any PIP requirements. You can still run the application inside Visual Studio without running this script (in debug mode or by removing the post build event), or collect the runtime dependencies yourself.