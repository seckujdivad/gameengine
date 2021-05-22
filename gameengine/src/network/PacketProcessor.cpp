#include "PacketProcessor.h"

#include <optional>

#include <wx/window.h>

#include "Packet.h"
#include "wx/PacketEvent.h"

void ProcessOutstandingPackets(EngineConnection& connection, wxWindow* emit_events_from)
{
	std::optional<Packet> latest_packet = connection.GetLatestPacket();
	while (latest_packet.has_value())
	{
		//handle this packet
		Packet& packet = latest_packet.value();
		if (packet.GetType() == Packet::Type::ConnEstablished)
		{
		}
		else if (packet.GetType() == Packet::Type::ChatMessage)
		{
		}
		else
		{
			throw std::runtime_error("Unknown packet type: " + std::to_string(static_cast<int>(packet.GetType())));
		}

		if (emit_events_from != nullptr)
		{
			PacketEvent event = PacketEvent(latest_packet.value(), emit_events_from);
			emit_events_from->ProcessWindowEvent(event);
		}

		//get the next packet
		latest_packet = connection.GetLatestPacket();
	}
}
