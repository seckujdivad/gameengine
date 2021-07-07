#include "EngineConnection.h"

#include <stdexcept>

#include "../events/EventHandler.h"

void EngineConnection::BytesReceived(std::vector<char> bytes) //CALLED FROM A SEPARATE THREAD
{
	this->m_events_lock.lock();
	const std::lock_guard<std::mutex> events_lock = std::lock_guard<std::mutex>(this->m_events_lock, std::adopt_lock);
	this->m_events.emplace(NetworkEvent::Type::PacketReceived, bytes);
}

EngineConnection::EngineConnection(ConnectionTarget target) : Connection(target)
{
	this->m_events_lock.lock();
	const std::lock_guard<std::mutex> events_lock = std::lock_guard<std::mutex>(this->m_events_lock, std::adopt_lock);
	this->m_events.emplace(NetworkEvent::Type::ConnEstablished, target);
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

std::optional<NetworkEvent> EngineConnection::GetLatestEvent()
{
	this->m_events_lock.lock();
	const std::lock_guard<std::mutex> events_lock = std::lock_guard<std::mutex>(this->m_events_lock, std::adopt_lock);
	if (!this->m_events.empty())
	{
		std::optional<NetworkEvent> result = this->m_events.front();
		this->m_events.pop();
		return result;
	}
	else
	{
		return std::optional<NetworkEvent>();
	}
}

bool EngineConnection::HasUnprocessedEvents()
{
	this->m_events_lock.lock();
	const std::lock_guard<std::mutex> events_lock = std::lock_guard<std::mutex>(this->m_events_lock, std::adopt_lock);
	return !this->m_events.empty();
}

std::size_t EngineConnection::GetNumUnprocessedEvents()
{
	this->m_events_lock.lock();
	const std::lock_guard<std::mutex> events_lock = std::lock_guard<std::mutex>(this->m_events_lock, std::adopt_lock);
	return this->m_events.size();
}

void EngineConnection::ProcessOutstandingPackets(std::vector<EventHandler*> handlers)
{
	std::optional<NetworkEvent> latest_event = this->GetLatestEvent();
	while (latest_event.has_value())
	{
		//handle this event
		NetworkEvent& event = latest_event.value();

		if (event.GetType() == NetworkEvent::Type::PacketReceived)
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

		for (const auto& handler : handlers)
		{
			handler->BroadcastEvent(event);
		}

		//get the next packet
		latest_event = this->GetLatestEvent();
	}
}