#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <string>

class Nameable
{
private:
	std::string m_identifier = "";

public:
	Nameable();
	Nameable(const Nameable& copyfrom);
	Nameable& operator=(Nameable& copyfrom);
	~Nameable();

	void SetIdentifier(std::string identifier);
	std::string GetIdentifier();
};