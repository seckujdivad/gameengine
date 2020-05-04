#include <wx/wxprec.h>
#include "Nameable.h"

Nameable::Nameable()
{
}

Nameable::Nameable(const Nameable& copyfrom)
{
	this->m_identifier = copyfrom.m_identifier;
}

Nameable& Nameable::operator=(Nameable& copyfrom)
{
	this->m_identifier = copyfrom.GetIdentifier();
	return *this;
}

Nameable::~Nameable()
{
}

void Nameable::SetIdentifier(std::string identifier)
{
	this->m_identifier = identifier;
}

std::string Nameable::GetIdentifier()
{
	return this->m_identifier;
}
