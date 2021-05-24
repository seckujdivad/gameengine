#include "EngineConnection.h"

#include <stdexcept>

#include <wx/window.h>

#include "wx/PacketEvent.h"

void EngineConnection::BytesReceived(std::vector<char> bytes) //CALLED FROM A SEPARATE THREAD
{
	this->m_packets_received_lock.lock();

	this->m_packets_received.emplace(bytes);

	this->m_packets_received_lock.unlock();
}

EngineConnection::EngineConnection(std::string address, unsigned short port) : Connection(address, port)
{
}

void EngineConnection::SendPacket(Packet packet)
{
	this->SendBytes(packet.Serialise());
}

std::optional<Packet> EngineConnection::GetLatestPacket()
{
	std::optional<Packet> result;

	this->m_packets_received_lock.lock();

	if (!this->m_packets_received.empty())
	{
		result = this->m_packets_received.front();
		this->m_packets_received.pop();
	}

	this->m_packets_received_lock.unlock();

	return result;
}

bool EngineConnection::HasUnprocessedPackets()
{
	this->m_packets_received_lock.lock();

	bool is_empty = this->m_packets_received.empty();

	this->m_packets_received_lock.unlock();

	return !is_empty;
}

std::size_t EngineConnection::GetNumUnprocessedPackets()
{
	this->m_packets_received_lock.lock();

	std::size_t num_packets = this->m_packets_received.size();

	this->m_packets_received_lock.unlock();

	return num_packets;
}

void EngineConnection::ProcessOutstandingPackets(wxWindow* emit_events_from)
{
	std::optional<Packet> latest_packet = this->GetLatestPacket();
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
		latest_packet = this->GetLatestPacket();
	}
}
