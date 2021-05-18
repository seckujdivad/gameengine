#include "EngineConnection.h"

#include <stdexcept>

void EngineConnection::BytesReceived(std::vector<unsigned char> bytes) //CALLED FROM A SEPARATE THREAD
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