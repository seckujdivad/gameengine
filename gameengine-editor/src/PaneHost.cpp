#include <wx/wxprec.h>
#include "PaneHost.h"

PaneHost::PaneHost(Main* parent) : wxPanel(parent)
{
	this->m_parent = parent;

	this->SetMinSize(wxSize(100, 100));

	this->m_aui_manager.SetManagedWindow(this);

	this->m_aui_manager.Update();
}

PaneHost::~PaneHost()
{
	this->m_aui_manager.UnInit();
}