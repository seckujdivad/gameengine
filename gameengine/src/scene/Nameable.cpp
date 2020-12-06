#include "Nameable.h"

Nameable::Nameable()
{
}

void Nameable::SetIdentifier(std::string identifier)
{
	this->m_identifier = identifier;
}

void Nameable::SetIdentifier(const char* identifier)
{
	this->m_identifier = std::string(identifier);
}


std::string Nameable::GetIdentifier() const
{
	return this->m_identifier;
};