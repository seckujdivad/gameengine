#pragma once

#include <wx/wx.h>

#include "Main.h"

class App : public wxApp
{
private:
	Main* m_frame_main;

public:
	bool OnInit();
};