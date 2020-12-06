#pragma once

#include <wx/panel.h>
#include <wx/spinctrl.h>
#include <wx/event.h>

#include <vector>

class wxGridBagSizer;

struct VectorCtrlConfig
{
	long int style = wxSP_ARROW_KEYS;
	int orient = wxHORIZONTAL;
	int num_fields = 3;
	double min = -10000.0;
	double max = 10000.0;
	double initial = 0.0;
	double inc = 1.0; //increment
	bool can_be_min = true;
	bool can_be_max = true;
};

class VectorCtrlEvent : public wxEvent
{
private:
	std::vector<double> m_values;

public:
	VectorCtrlEvent(wxEventType eventType, int winid, std::vector<double> values);

	virtual wxEvent* Clone() const;

	std::vector<double> GetValues() const;
	double GetValue(int index) const;
};

#define VectorCtrlEventHandler(func) (&func)

wxDECLARE_EVENT(EVT_VCTRCTRL_CHANGED, VectorCtrlEvent);

/*
wxWidgets control for displaying and editing multiple doubles

Exposes functionality from https://docs.wxwidgets.org/3.0/classwx_spin_ctrl_double.html
*/
class VectorCtrl : public wxPanel
{
private:
	wxGridBagSizer* m_sizer;

	std::vector<wxSpinCtrlDouble*> m_spndbl_fields;

	void evt_OnValuesChanged(wxSpinDoubleEvent& evt);

	bool m_can_be_min;
	bool m_can_be_max;

public:
	VectorCtrl(
		wxWindow* parent,
		wxWindowID winid = wxID_ANY,
		VectorCtrlConfig config = {},
		const wxPoint& pos = wxDefaultPosition,
		const wxSize& size = wxDefaultSize,
		long int style = wxTAB_TRAVERSAL | wxNO_BORDER,
		const wxString& name = "VectorCtrl"
		);

	//analagous to wxSpinCtrlDouble methods logically modified to operate over multiple wxSpinCtrlDoubles
	std::vector<int> GetDigits() const;
	double GetIncrement() const;
	double GetMax() const;
	double GetMin() const;
	std::vector<double> GetValues() const;
	void SetDigits(std::vector<int> digits);
	void SetDigits(int digits);
	void SetIncrement(double inc);
	void SetRange(double minVal, double maxVal);
	void SetValues(std::vector<double> values);
	void SetValues(double value);

	//extended functionality
	void SetMax(double max_value);
	void SetMin(double min_value);

	void ValuesCanBeMin(bool can_be_min);
	void ValuesCanBeMax(bool can_be_max);
};