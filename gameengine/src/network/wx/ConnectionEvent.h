#pragma once

#include <variant>
#include <tuple>

#include <wx/event.h>

#include "../Packet.h"
#include "../EngineConnectionEvent.h"

class ConnectionEvent : public wxEvent
{
private:
	EngineConnectionEvent m_data;

public:
	ConnectionEvent(EngineConnectionEvent data, wxWindow* window = nullptr);

	const EngineConnectionEvent& GetEvent() const;

	//wxWidgets RTTI
	wxEvent* Clone() const override;
};

#define ConnectionEventHandler(func) (&func)

wxDECLARE_EVENT(EVT_CONNECTION, ConnectionEvent);