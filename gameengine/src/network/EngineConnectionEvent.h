#pragma once

#include <variant>

#include "Packet.h"
#include "ConnectionTarget.h"

class EngineConnectionEvent
{
public:
	enum class Type
	{
		ConnEstablished,
		ConnClosed,
		PacketReceived
	};

private:
	std::variant<Packet, ConnectionTarget> m_data;
	Type m_type;

public:
	EngineConnectionEvent(Type type, std::variant<Packet, ConnectionTarget> data);

	Type GetType() const;

	const Packet& GetPacket() const;
	const ConnectionTarget& GetConnectionTarget() const;
};