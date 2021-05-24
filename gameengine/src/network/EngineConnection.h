#pragma once

#include <queue>
#include <optional>
#include <mutex>

#include "Connection.h"
#include "Packet.h"

class wxWindow;

class EngineConnection : public Connection
{
private:
	std::mutex m_packets_received_lock;
	std::queue<Packet> m_packets_received;

protected:
	void BytesReceived(std::vector<char> bytes) override;

public:
	EngineConnection(std::string address, unsigned short port);

	void SendPacket(Packet packet);
	std::optional<Packet> GetLatestPacket();
	bool HasUnprocessedPackets();
	std::size_t GetNumUnprocessedPackets();

	void ProcessOutstandingPackets(wxWindow* emit_events_from = nullptr);
};