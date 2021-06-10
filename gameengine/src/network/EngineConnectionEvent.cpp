#include "EngineConnectionEvent.h"

#include <stdexcept>

EngineConnectionEvent::EngineConnectionEvent(Type type, std::variant<Packet, ConnectionTarget> data) : m_type(type), m_data(data)
{
	if (type == Type::ConnEstablished || type == Type::ConnClosed)
	{
		if (data.index() != 1)
		{
			throw std::invalid_argument("When 'type' is Type::ConnEstablished or Type::ConnClosed, a Connection::Target must be provided");
		}
	}
	else if (type == Type::PacketReceived)
	{
		if (data.index() != 0)
		{
			throw std::invalid_argument("When 'type' is Type::PacketReceived, a Packet must be provided");
		}
	}
	else
	{
		throw std::invalid_argument("Unknown type");
	}
}

EngineConnectionEvent::Type EngineConnectionEvent::GetType() const
{
	return this->m_type;
}

const Packet& EngineConnectionEvent::GetPacket() const
{
	return std::get<Packet>(this->m_data);
}

const ConnectionTarget& EngineConnectionEvent::GetConnectionTarget() const
{
	return std::get<ConnectionTarget>(this->m_data);
}
