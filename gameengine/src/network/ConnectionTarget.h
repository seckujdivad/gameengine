#pragma once

#include <string>

struct ConnectionTarget
{
	ConnectionTarget(std::string address, unsigned short port);

	std::string Display() const;

	std::string address;
	unsigned short port;
};