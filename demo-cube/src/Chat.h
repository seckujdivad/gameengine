#pragma once

#include <memory>

#include <wx/frame.h>

class wxGridBagSizer;
class wxButton;
class wxListBox;
class wxTextCtrl;

class EngineConnection;

class Chat : public wxFrame
{
private:
	wxGridBagSizer* m_sizer;
	wxTextCtrl* m_txt_message;
	wxListBox* m_lb_messages;

	void txt_message_KeyPressed(wxKeyEvent& evt);

	std::shared_ptr<EngineConnection> m_connection;

public:
	Chat(wxWindow* parent, std::shared_ptr<EngineConnection> connection);
};