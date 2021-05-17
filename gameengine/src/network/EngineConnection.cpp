#include "EngineConnection.h"

#include <stdexcept>

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