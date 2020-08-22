#include <wx/wxprec.h>
#include "App.h"

wxIMPLEMENT_APP(App);

bool App::OnInit()
{
#ifdef PAUSE_ON_RUN
	wxMessageBox("Inject debuggers (RenderDoc etc) if you are using them into this process before clicking OK", "Inject debuggers");
#endif

#ifdef _DEBUG
	_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif

	wxInitAllImageHandlers();

	this->m_frame_main = new Main();
	this->m_frame_main->Show();

	return true;
}

int App::OnExit()
{
	return 0;
}