#pragma once

#include <memory>

#include <wx/frame.h>

#include "network/NetworkEvent.h"

class wxGridBagSizer;
class wxButton;
class wxListBox;
class wxTextCtrl;

class Main;

class EngineConnection;

class Chat : public wxFrame
{
private:
	wxGridBagSizer* m_sizer;
	wxTextCtrl* m_txt_message;
	wxListBox* m_lb_messages;

	void txt_message_KeyPressed(wxKeyEvent& evt);

	void OnNetworkEvent(const NetworkEvent& evt);

	Main* GetParent() const;

public:
	Chat(Main* parent);
};