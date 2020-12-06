#pragma once

#include <string>

class Nameable
{
private:
	std::string m_identifier = "";

public:
	Nameable();

	void SetIdentifier(std::string identifier);
	void SetIdentifier(const char* identifier);

	std::string GetIdentifier() const;
};