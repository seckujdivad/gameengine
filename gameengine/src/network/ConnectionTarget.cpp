#include "ConnectionTarget.h"

ConnectionTarget::ConnectionTarget(std::string address, unsigned short port) : address(address), port(port)
{
}

std::string ConnectionTarget::Display() const
{
	return this->address + ":" + std::to_string(this->port);
}
