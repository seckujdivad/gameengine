#include "ThreadNamer.h"

#ifdef _WIN32
#include <windows.h>

#endif

void NameThread(std::string name)
{
#ifdef _WIN32
	std::wstring wide_name = std::wstring(name.begin(), name.end());
	NameThread(wide_name);
#endif
}

void NameThread(std::wstring name)
{
#ifdef _WIN32
	//https://docs.microsoft.com/en-us/visualstudio/debugger/how-to-set-a-thread-name-in-native-code?view=vs-2019
	SetThreadDescription(GetCurrentThread(), name.c_str());
#endif
}
