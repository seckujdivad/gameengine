#include "EngineConnection.h"

#include <stdexcept>

#include <wx/window.h>

void EngineConnection::BytesReceived(std::vector<char> bytes) //CALLED FROM A SEPARATE THREAD
{
	this->m_events_lock.lock();
	this->m_events.emplace(EngineConnectionEvent::Type::PacketReceived, bytes);
	this->m_events_lock.unlock();
}

EngineConnection::EngineConnection(ConnectionTarget target) : Connection(target)
{
	this->m_events_lock.lock();
	this->m_events.emplace(EngineConnectionEvent::Type::ConnEstablished, target);
	this->m_events_lock.unlock();
}

void EngineConnection::SendPacket(Packet packet)
{
	bool packet_type_sendable = false;
	for (Packet::Type sendable_type : SENDABLE_PACKET_TYPES)
	{
		if (sendable_type == packet.GetType())
		{
			packet_type_sendable = true;
		}
	}

	this->SendBytes(packet.Serialise());
}

std::optional<EngineConnectionEvent> EngineConnection::GetLatestEvent()
{
	std::optional<EngineConnectionEvent> result;

	this->m_events_lock.lock();
	if (!this->m_events.empty())
	{
		result = this->m_events.front();
		this->m_events.pop();
	}
	this->m_events_lock.unlock();

	return result;
}

bool EngineConnection::HasUnprocessedEvents()
{
	this->m_events_lock.lock();
	bool is_empty = this->m_events.empty();
	this->m_events_lock.unlock();

	return !is_empty;
}

std::size_t EngineConnection::GetNumUnprocessedEvents()
{
	this->m_events_lock.lock();
	std::size_t num_packets = this->m_events.size();
	this->m_events_lock.unlock();

	return num_packets;
}

void EngineConnection::ProcessOutstandingPackets(wxWindow* emit_events_from)
{
	std::optional<EngineConnectionEvent> latest_event = this->GetLatestEvent();
	while (latest_event.has_value())
	{
		//handle this event
		EngineConnectionEvent& event = latest_event.value();

		if (event.GetType() == EngineConnectionEvent::Type::PacketReceived)
		{
			const Packet& packet = event.GetPacket();

			bool packet_type_receivable = false;
			for (Packet::Type receivable_type : RECEIVABLE_PACKET_TYPES)
			{
				if (receivable_type == packet.GetType())
				{
					packet_type_receivable = true;
				}
			}

			if (!packet_type_receivable)
			{
				throw std::runtime_error("Packet type " + std::to_string(static_cast<int>(packet.GetType())) + " not found in list of receivable packets");
			}
		}

		if (emit_events_from != nullptr)
		{
			ConnectionEvent wx_event = ConnectionEvent(event, emit_events_from);
			emit_events_from->ProcessWindowEvent(wx_event);
		}

		//get the next packet
		latest_event = this->GetLatestEvent();
	}
}