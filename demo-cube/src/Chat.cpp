#include "Chat.h"

#include <wx/gbsizer.h>
#include <wx/textctrl.h>
#include <wx/listbox.h>

#include "network/EngineConnection.h"
#include "network/Packet.h"

void Chat::txt_message_KeyPressed(wxKeyEvent& evt)
{
	if (evt.GetKeyCode() == WXK_RETURN)
	{
		std::string to_send = this->m_txt_message->GetValue();
		this->m_connection->SendPacket(Packet(Packet::ChatMessage(to_send)));
		this->m_txt_message->SetValue("");
	}
	else
	{
		evt.Skip();
	}
}

void Chat::OnPacketReceived(PacketEvent& evt)
{
	const Packet& packet = evt.GetPacket();
	if (packet.GetType() == Packet::Type::ChatMessage)
	{
		this->m_lb_messages->AppendString(packet.GetData<Packet::ChatMessage>().message);
	}
}

Chat::Chat(wxWindow* parent, std::shared_ptr<EngineConnection> connection) : wxFrame(parent, wxID_ANY, "Chat"), m_connection(connection)
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

	parent->Bind(EVT_PACKET, &Chat::OnPacketReceived, this);
}
