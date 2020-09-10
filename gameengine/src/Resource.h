#pragma once

#ifdef _WIN32 //according to my GCC install on windows, this is still defined (ie not exclusive to MSVC++)
	#include <Windows.h>
	#include <WinBase.h>
	#include <atlbase.h>
#else
	#error Resource embedding is only implemented through WinAPI
#endif

#include <string>
#include <string_view>

#include "../resource.h"

class Resource
{
public:
	struct Parameters
	{
		size_t size_bytes = 0;
		void* pointer = nullptr;
	};

private:
	HRSRC m_resource = nullptr;
	HGLOBAL m_memory = nullptr;
	
	Parameters m_parameters;

public:
	Resource(int resource_id, int resource_type);

	std::string_view GetStringResource();
};

std::string GetEmbeddedTextfile(int resource_id);