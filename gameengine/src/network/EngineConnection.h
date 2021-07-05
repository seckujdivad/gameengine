#pragma once

#include <queue>
#include <optional>
#include <mutex>
#include <vector>
#include <memory>

#include "Connection.h"
#include "Packet.h"
#include "ConnectionTarget.h"
#include "NetworkEvent.h"

class EventHandler;

class EngineConnection : public Connection
{
private:
	std::mutex m_events_lock;
	std::queue<NetworkEvent> m_events;

protected:
	void BytesReceived(std::vector<char> bytes) override;

public:
	EngineConnection(ConnectionTarget target);

	void SendPacket(Packet packet);
	std::optional<NetworkEvent> GetLatestEvent();
	bool HasUnprocessedEvents();
	std::size_t GetNumUnprocessedEvents();

	void ProcessOutstandingPackets(std::vector<EventHandler*> handlers);
};