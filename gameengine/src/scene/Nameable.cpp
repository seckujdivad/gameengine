#include "Nameable.h"

Nameable::Nameable()
{
}

void Nameable::SetIdentifier(std::string identifier)
{
	this->m_identifier = identifier;
}


std::string Nameable::GetIdentifier() const
{
	return this->m_identifier;
};