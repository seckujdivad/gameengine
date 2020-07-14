#pragma once

#include <wx/wxprec.h>
#include <wx/wx.h>

#ifdef _DEBUG
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#endif

class Main;

#include "Main.h"

class App : public wxApp
{
private:
	Main* m_frame_main;

public:
	bool OnInit();
};