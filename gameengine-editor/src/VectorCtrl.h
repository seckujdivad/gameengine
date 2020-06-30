#pragma once

#include <wx/wx.h>
#include <wx/panel.h>
#include <wx/spinctrl.h>
#include <wx/gbsizer.h>
#include <wx/event.h>

#include <glm/glm.hpp>

#include <vector>
#include <stdexcept>

class VectorCtrlEvent : public wxEvent
{
private:
	std::vector<double> m_values;

public:
	VectorCtrlEvent(wxEventType eventType, int winid, std::vector<double> values);

	virtual wxEvent* Clone() const;

	std::vector<double> GetValues();
	double GetValue(int index);
};

#define VectorCtrlEventHandler(func) (&func)

wxDECLARE_EVENT(EVT_VCTRCTRL_CHANGED, VectorCtrlEvent);

/*
Exposes functionality from https://docs.wxwidgets.org/3.0/classwx_spin_ctrl_double.html
*/
class VectorCtrl : public wxPanel
{
private:
	wxGridBagSizer* m_sizer;

	std::vector<wxSpinCtrlDouble*> m_spndbl_fields;

	void evt_OnValuesChanged(wxSpinDoubleEvent& evt);

public:
	VectorCtrl(
		wxWindow* parent,
		wxWindowID winid = wxID_ANY,
		int num_fields = 3,
		int orient = wxHORIZONTAL,
		const wxPoint& pos = wxDefaultPosition,
		const wxSize& size = wxDefaultSize,
		long style = wxTAB_TRAVERSAL | wxNO_BORDER,
		const wxString& name = "VectorCtrl"
		);

	std::vector<int> GetDigits();
	double GetIncrement();
	double GetMax();
	double GetMin();
	std::vector<double> GetValues();
	void SetDigits(std::vector<int> digits);
	void SetDigits(int digits);
	void SetIncrement(double inc);
	void SetRange(double minVal, double maxVal);
	void SetValues(std::vector<double> values);
	void SetValues(double value);
};