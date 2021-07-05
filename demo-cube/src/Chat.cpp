#include "Chat.h"

#include <wx/gbsizer.h>
#include <wx/textctrl.h>
#include <wx/listbox.h>
#include <wx/msgdlg.h>

#include "network/EngineConnection.h"
#include "network/Packet.h"

#include "Main.h"

void Chat::txt_message_KeyPressed(wxKeyEvent& evt)
{
	if (evt.GetKeyCode() == WXK_RETURN)
	{
		if (this->GetParent()->IsConnected())
		{
			std::string to_send = this->m_txt_message->GetValue();
			this->GetParent()->GetConnection()->SendPacket(Packet(Packet::ClientChatMessage(to_send)));
		}
		else
		{
			wxMessageBox("Must be connected to send a message", "Not connected");
		}
		
		this->m_txt_message->SetValue("");
	}
	else
	{
		evt.Skip();
	}
}

void Chat::OnNetworkEvent(const NetworkEvent& evt)
{
	if (evt.GetType() == NetworkEvent::Type::PacketReceived)
	{
		const Packet& packet = evt.GetPacket();
		if (packet.GetType() == Packet::Type::ServerChatMessage)
		{
			this->m_lb_messages->AppendString(packet.GetData<Packet::ServerChatMessage>().name + ": " + packet.GetData<Packet::ServerChatMessage>().message);
		}
	}
}

Main* Chat::GetParent() const
{
	return reinterpret_cast<Main*>(this->m_parent);
}

Chat::Chat(Main* parent) : wxFrame(parent, wxID_ANY, "Chat")
{
	this->m_sizer = new wxGridBagSizer(0, 0);
	this->m_sizer->SetFlexibleDirection(wxBOTH);
	this->m_sizer->SetNonFlexibleGrowMode(wxFLEX_GROWMODE_SPECIFIED);

	this->m_lb_messages = new wxListBox(this, wxID_ANY);
	this->m_sizer->Add(this->m_lb_messages, wxGBPosition(0, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_txt_message = new wxTextCtrl(this, wxID_ANY);
	this->m_sizer->Add(this->m_txt_message, wxGBPosition(1, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);
	this->m_txt_message->Bind(wxEVT_CHAR, &Chat::txt_message_KeyPressed, this);

	this->m_sizer->AddGrowableRow(0);
	this->m_sizer->AddGrowableCol(0);

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Layout();

	parent->GetEventHandler().BindToEvent<NetworkEvent>(std::bind(&Chat::OnNetworkEvent, this, std::placeholders::_1));
}
