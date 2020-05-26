#include "Resource.h"

Resource::Resource(int resource_id, int resource_type)
{
	this->m_resource = FindResourceW(GetModuleHandleW(nullptr), MAKEINTRESOURCEW(resource_id), MAKEINTRESOURCEW(resource_type));
	this->m_memory = LoadResource(nullptr, this->m_resource);

	if (this->m_resource == nullptr)
	{
		int last_err_code = GetLastError();
		throw std::runtime_error("Couldn't locate resource ID " + std::to_string(resource_id) + " (type ID " + std::to_string(resource_type) + ", error code " + std::to_string(last_err_code) + "). Make sure the appropriate resource files are included in this application's resource file");
	}

	this->m_parameters.size_bytes = SizeofResource(nullptr, this->m_resource);
	this->m_parameters.pointer = LockResource(this->m_memory);
}

std::string_view Resource::GetStringResource()
{
	std::string_view result;
	if (this->m_parameters.pointer != nullptr)
	{
		result = std::string_view(reinterpret_cast<char*>(this->m_parameters.pointer), this->m_parameters.size_bytes);
	}

	return result;
}

std::string GetEmbeddedTextfile(int resource_id)
{
	Resource resource = Resource(resource_id, RCT_TEXTFILE);
	return std::string(resource.GetStringResource());
}