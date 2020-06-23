#pragma once

#include <wx/wxprec.h>
#include "../GLComponents.h"

#include <string>

#include "../EventEmitter.h"
#include "../EventManager.h"

class Nameable : public virtual EventEmitter
{
private:
	std::string m_identifier = "";

public:
	Nameable(EventManager* evtman);
	Nameable(const Nameable& copyfrom);
	Nameable& operator=(Nameable& copyfrom);
	~Nameable();

	void SetIdentifier(std::string identifier);
	virtual std::string GetIdentifier();
};