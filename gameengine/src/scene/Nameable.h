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

	void SetIdentifier(std::string identifier);
	virtual std::string GetIdentifier();
};