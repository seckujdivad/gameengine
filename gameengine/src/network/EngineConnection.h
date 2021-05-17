#pragma once

#include <string>
#include <atomic>

#include <stdint.h>

#include "Connection.h"

class EngineConnection : public Connection
{
private:
	std::atomic<int32_t> m_uid;

protected:
	void BytesReceived(std::vector<unsigned char> bytes) override;

public:
	EngineConnection(std::string address, unsigned short port);
};