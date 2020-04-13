OpenGL renderer using wxWidgets components.

## Installation
An x86 installer is provided, but it isn't as thoroughly tested as the zip file. It shouldn't break in a way that damages anything (it's provided by Visual Studio) but it might not properly unpack all of the dependencies. If files seem to be missing, file an issue and try the zipped releases.

## Dependencies
### vcpkg
* nlohmann-json
* wxwidgets
* glm
* glew
* opengl

This project statically links to wxWidgets. vcpkg builds the dynamic version by default, so you will need to edit the CMake files for your platform triplets. On 64-bit Windows, the triplet is `x64-windows`. Therefore, you will need to edit the file at `vcpkg/triplets/x64-windows.cmake` before building and add the following to the end before building through vcpkg:
```cmake
if (PORT STREQUAL wxwidgets)
	set(VCPKG_LIBRARY_LINKAGE static)
endif()
```

### NuGet
* Google Test

Google Test is only required for building and running the unit tests. If you don't want to use these, it is not required.

64-bit compilation is supported, but you will need to download 64-bit versions of the vcpkg dependencies to your machine. vcpkg can be downloaded and set up from [here](https://docs.microsoft.com/en-us/cpp/build/vcpkg).