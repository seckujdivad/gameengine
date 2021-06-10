#pragma once

#include <queue>
#include <optional>
#include <mutex>

#include "Connection.h"
#include "Packet.h"
#include "wx/ConnectionEvent.h"

class wxWindow;

class EngineConnection : public Connection
{
private:
	std::mutex m_events_lock;
	std::queue<EngineConnectionEvent> m_events;

protected:
	void BytesReceived(std::vector<char> bytes) override;

public:
	EngineConnection(std::string address, unsigned short port);

	void SendPacket(Packet packet);
	std::optional<EngineConnectionEvent> GetLatestEvent();
	bool HasUnprocessedEvents();
	std::size_t GetNumUnprocessedEvents();

	void ProcessOutstandingPackets(wxWindow* emit_events_from = nullptr);
};