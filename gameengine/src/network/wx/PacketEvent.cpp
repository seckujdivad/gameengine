#include "PacketEvent.h"

#include <wx/window.h>

wxDEFINE_EVENT(EVT_PACKET, PacketEvent);

PacketEvent::PacketEvent(Packet packet, wxWindow* window) : wxEvent(window->GetId(), EVT_PACKET), m_packet(packet)
{
	
}

const Packet& PacketEvent::GetPacket() const
{
	return this->m_packet;
}

wxEvent* PacketEvent::Clone() const
{
	return new PacketEvent(*this);
}
