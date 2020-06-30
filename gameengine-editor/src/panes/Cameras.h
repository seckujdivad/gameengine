#pragma once

#include <wx/wx.h>
#include <wx/gbsizer.h>
#include <wx/listbox.h>
#include <wx/button.h>
#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/spinctrl.h>
#include <wx/stattext.h>

#include <string>
#include <vector>

#include "scene/Scene.h"
#include "scene/Camera.h"

#include "Pane.h"
#include "../PaneHost.h"
#include "../VectorCtrl.h"

class Cameras : public Pane
{
private:
	wxGridBagSizer* m_sizer;

	wxListBox* m_lb_cameras;

	wxTextCtrl* m_txt_name;

	wxStaticText* m_stxt_fov;
	wxSpinCtrlDouble* m_spndbl_fov;

	wxStaticText* m_stxt_pos;
	VectorCtrl* m_vct_pos;

	wxStaticText* m_stxt_rot;
	VectorCtrl* m_vct_rot;

	wxStaticText* m_stxt_clips;
	VectorCtrl* m_vct_clips;

	void evt_SelectionChanged(wxCommandEvent& evt);
	void evt_NameChanged(wxCommandEvent& evt);
	void evt_FOVChanged(wxSpinDoubleEvent& evt);
	void evt_PosChanged(VectorCtrlEvent& evt);
	void evt_RotChanged(VectorCtrlEvent& evt);
	void evt_ClipsChanged(VectorCtrlEvent& evt);

	std::vector<std::string> m_camera_names;

public:
	Cameras(PaneHost* parent);

	std::string GetDisplayName();
	void SceneChangedEvent(Scene* scene);
};