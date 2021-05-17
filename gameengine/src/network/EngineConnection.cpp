#include "EngineConnection.h"

#include <stdexcept>
#include <stdint.h>

enum class PacketType
{
	ConnEstablished,
	ChatMessage
};

void EngineConnection::BytesReceived(std::vector<unsigned char> bytes) //CALLED FROM A SEPARATE THREAD
{
	if (bytes.size() == 0)
	{
		throw std::runtime_error("Empty message decoded");
	}
	else
	{
		PacketType msg_type = static_cast<PacketType>(bytes.at(0));
		
		if (msg_type == PacketType::ConnEstablished)
		{
			if (bytes.size() == 10)
			{
				int64_t uid = *reinterpret_cast<int64_t*>(bytes.data() + sizeof(unsigned char));
			}
			else
			{
				throw std::runtime_error("Unexpected ConnEstablished packet size");
			}
		}
		else if (msg_type == PacketType::ChatMessage)
		{

		}
		else
		{
			throw std::runtime_error("Unknown packet header received from server");
		}
	}
}

EngineConnection::EngineConnection(std::string address, unsigned short port) : Connection(address, port)
{
}