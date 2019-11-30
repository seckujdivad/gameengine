#pragma once

#include <wx/wx.h>

#include "wxfrontend/Main.h"

class App : public wxApp
{
private:
	Main* m_frame_main;

public:
	bool OnInit();
};