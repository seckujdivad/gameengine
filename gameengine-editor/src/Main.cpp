#include <wx/wxprec.h>
#include "Main.h"

void Main::tlbr_btn_AddPane_clicked(wxCommandEvent& evt)
{
	wxButton* button = (wxButton*)evt.GetEventObject();
	if (button->GetLabelText() == "Viewport")
	{
		this->m_panehost->AddPane<Viewport>(wxAUI_DOCK_BOTTOM);
	}
	else if (button->GetLabelText() == "Lights")
	{
		this->m_panehost->AddPane<Lights>(wxAUI_DOCK_BOTTOM);
	}

	//evt.Skip(); //don't skip as it seems to call this function again (maybe it's the toolbar doing it?)
}

void Main::menubar_item_selected(wxCommandEvent& evt)
{
	std::string selection_name = ((wxMenu*)evt.GetEventObject())->FindItem(evt.GetId(), nullptr)->GetName();

#ifdef _DEBUG
	wxString default_dir = "../demo-cube/resources/";
#else
	wxString default_dir = wxEmptyString;
#endif

	wxFileDialog* file_dialog = new wxFileDialog(this, "Select scene", wxEmptyString, default_dir, "*.json");
	int result = file_dialog->ShowModal();

	if (result == wxID_OK)
	{
		this->m_panehost->LoadScene(file_dialog->GetPath());
	}

	delete file_dialog;
}

Main::Main() : wxFrame(nullptr, wxID_ANY, "Level Editor")
{
	this->m_sizer = new wxGridBagSizer(0, 0);
	this->m_sizer->SetFlexibleDirection(wxBOTH);
	this->m_sizer->SetNonFlexibleGrowMode(wxFLEX_GROWMODE_SPECIFIED);

	//create toolbar
	this->m_toolbar = this->CreateToolBar(wxTB_FLAT | wxTB_HORIZONTAL);
	
	std::vector<std::string> panes = {
		"Viewport",
		"Lights"
	};

	wxButton* button;
	for (int i = 0; i < (int)panes.size(); i++)
	{
		button = new wxButton(this->m_toolbar, wxID_ANY, panes.at(i));
		button->Bind(wxEVT_BUTTON, &Main::tlbr_btn_AddPane_clicked, this);
		this->m_toolbar->AddControl(button);
	}

	this->m_toolbar->Realize();

	//create menu bar
	this->m_menubar = new wxMenuBar();

	wxMenu* menu;

	menu = new wxMenu();
	wxMenuItem* item = menu->Append(wxID_OPEN, "Open");
	this->m_menubar->Append(menu, "File");
	this->m_menus.push_back(menu);

	this->Bind(wxEVT_MENU, &Main::menubar_item_selected, this, item->GetId());
	this->SetMenuBar(this->m_menubar);
	
	//create pane host
	this->m_panehost = new PaneHost(this);
	this->m_sizer->Add(this->m_panehost, wxGBPosition(0, 0), wxGBSpan(1, 1), wxEXPAND | wxALL);

	this->m_sizer->AddGrowableRow(0);
	this->m_sizer->AddGrowableCol(0);
	
	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Show();
}

Main::~Main()
{
	
}