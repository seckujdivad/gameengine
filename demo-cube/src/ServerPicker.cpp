#include "ServerPicker.h"

#include <stdexcept>
#include <optional>

#include <wx/gbsizer.h>
#include <wx/listbox.h>

#include "Main.h"
#include "Settings.h"

void ServerPicker::lb_servers_DoubleClicked(wxCommandEvent& evt)
{
	int index = evt.GetInt();
	if (index >= 0 && index < static_cast<int>(this->m_servers.size()))
	{
		this->GetParent()->ConnectTo(this->m_servers.at(index));
	}
	else
	{
		throw std::runtime_error("Returned index out of range");
	}

	evt.Skip();
}

void ServerPicker::OnNetworkEvent(const NetworkEvent& evt)
{
	if (evt.GetType() == NetworkEvent::Type::ConnEstablished)
	{
		this->Close();
	}
}

void ServerPicker::RefreshServers()
{
	this->m_servers.clear();
	this->m_lb_servers->Clear();

	if (this->GetParent()->GetSettings().contains("network")
		&& this->GetParent()->GetSettings()["network"].is_object()
		&& this->GetParent()->GetSettings()["network"].contains("servers")
		&& this->GetParent()->GetSettings()["network"]["servers"].is_array())
	{
		for (const nlohmann::json& server_info : this->GetParent()->GetSettings()["network"]["servers"])
		{
			const auto& [name, target] = GetConnectionTarget(server_info);

			std::string destination = target.address + ":" + std::to_string(target.port);
			if (name.has_value())
			{
				this->m_lb_servers->AppendString(name.value() + " (" + target.Display() + ")");
			}
			else
			{
				this->m_lb_servers->AppendString(destination);
			}

			this->m_servers.push_back(target);
		}
	}
}

Main* ServerPicker::GetParent() const
{
	return reinterpret_cast<Main*>(this->m_parent);
}

ServerPicker::ServerPicker(Main* parent) : wxFrame(parent, wxID_ANY, "Server picker")
{
	this->m_sizer = new wxGridBagSizer(0, 0);
	this->m_sizer->SetFlexibleDirection(wxBOTH);
	this->m_sizer->SetNonFlexibleGrowMode(wxFLEX_GROWMODE_SPECIFIED);

	this->m_lb_servers = new wxListBox(this, wxID_ANY);
	this->m_lb_servers->Bind(wxEVT_LISTBOX_DCLICK, &ServerPicker::lb_servers_DoubleClicked, this);
	this->RefreshServers();
	this->m_sizer->Add(this->m_lb_servers, wxGBPosition(0, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_sizer->AddGrowableRow(0);
	this->m_sizer->AddGrowableCol(0);

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Layout();

	parent->GetEventHandler().BindToEvent<NetworkEvent>(&ServerPicker::OnNetworkEvent, this);
}
