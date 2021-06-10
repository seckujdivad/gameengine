#pragma once

#include <string>
#include <vector>

#include <wx/frame.h>

class wxListBox;
class wxGridBagSizer;

class Main;

class ServerPicker : public wxFrame
{
private:
	struct Server
	{
		inline Server(std::string address, int port) : address(address), port(port) {};

		std::string address;
		int port;
	};

	std::vector<Server> m_servers;

	wxGridBagSizer* m_sizer = nullptr;
	wxListBox* m_lb_servers = nullptr;

	void lb_servers_DoubleClicked(wxCommandEvent& evt);

	void RefreshServers();

	Main* GetParent() const;

public:
	ServerPicker(Main* parent);
};