#pragma once

#include <string>
#include <vector>

#include <wx/frame.h>

#include "network/ConnectionTarget.h"
#include "network/wx/ConnectionEvent.h"

class wxListBox;
class wxGridBagSizer;

class Main;

class ServerPicker : public wxFrame
{
private:
	std::vector<ConnectionTarget> m_servers;

	wxGridBagSizer* m_sizer = nullptr;
	wxListBox* m_lb_servers = nullptr;

	void lb_servers_DoubleClicked(wxCommandEvent& evt);
	void OnConnectionEvent(ConnectionEvent& evt);

	void RefreshServers();

	Main* GetParent() const;

public:
	ServerPicker(Main* parent);
};