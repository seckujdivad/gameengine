#include "App.h"

#include <string>

#include <wx/msgdlg.h>

#include "Main.h"

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

void App::OnUnhandledException()
{
#ifdef _DEBUG
	throw;
#else
	std::string err_message;
	try
	{
		throw;
	}
	catch (std::exception& e)
	{
		err_message.append("Standard exception was uncaught\nChoose OK to exit, Cancel to debug\nType: " + std::string(typeid(e).name()) + "\n" + e.what());
	}
	catch (...)
	{
		err_message.append("Unknown exception was uncaught\nChoose OK to exit, Cancel to debug");
	}

	wxMessageDialog err_dialog = wxMessageDialog(nullptr, err_message, "Uncaught exception", wxOK | wxCANCEL | wxICON_ERROR | wxCENTRE);

	int result = err_dialog.ShowModal();
	if (result == wxID_CANCEL)
	{
		throw;
	}
#endif
}
