#include <wx/wxprec.h>
#include "App.h"

wxIMPLEMENT_APP(App);

bool App::OnInit()
{
#ifdef _DEBUG
	wxMessageBox("Inject debuggers (RenderDoc etc) if you are using them into this process before clicking OK", "Inject debuggers");
#endif

	wxInitAllImageHandlers();

	this->m_frame_main = new Main();
	this->m_frame_main->Show();

	return true;
}