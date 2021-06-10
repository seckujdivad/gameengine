#include "ConnectionEvent.h"

#include <wx/window.h>

wxDEFINE_EVENT(EVT_CONNECTION, ConnectionEvent);

ConnectionEvent::ConnectionEvent(EngineConnectionEvent data, wxWindow* window) : wxEvent(window->GetId(), EVT_CONNECTION), m_data(data)
{
}

const EngineConnectionEvent& ConnectionEvent::GetEvent() const
{
	return this->m_data;
}

wxEvent* ConnectionEvent::Clone() const
{
	return new ConnectionEvent(*this);
}
