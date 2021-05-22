#pragma once

#include <wx/event.h>

#include "../Packet.h"

class PacketEvent : public wxEvent
{
private:
	Packet m_packet;

public:
	PacketEvent(Packet packet, wxWindow* window = nullptr);

	const Packet& GetPacket() const;

	//wxWidgets RTTI
	wxEvent* Clone() const override;
};

#define PacketEventHandler(func) (&func)

wxDECLARE_EVENT(EVT_PACKET, PacketEvent);