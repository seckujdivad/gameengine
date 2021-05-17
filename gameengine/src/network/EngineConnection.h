#pragma once

#include <string>

#include "Connection.h"

class EngineConnection : public Connection
{
private:
protected:
	void BytesReceived(std::vector<unsigned char> bytes) override;

public:
	EngineConnection(std::string address, unsigned short port);
};