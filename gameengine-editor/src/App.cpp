#include <wx/wxprec.h>
#include "App.h"

wxIMPLEMENT_APP(App);

bool App::OnInit()
{
	wxInitAllImageHandlers();

	this->m_frame_main = new Main();
	this->m_frame_main->Show();

	return true;
}