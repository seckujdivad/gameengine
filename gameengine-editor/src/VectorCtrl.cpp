#include "VectorCtrl.h"

wxDEFINE_EVENT(EVT_VCTRCTRL_CHANGED, VectorCtrlEvent);

void VectorCtrl::evt_OnValuesChanged(wxSpinDoubleEvent& evt)
{
	VectorCtrlEvent event(EVT_VCTRCTRL_CHANGED, GetId(), this->GetValues());
	event.SetEventObject(this);
	ProcessWindowEvent(event);

	evt.Skip();
}

VectorCtrl::VectorCtrl(wxWindow* parent, wxWindowID winid, int num_fields, int orient, const wxPoint& pos, const wxSize& size, long style, const wxString& name) : wxPanel(parent, winid, pos, size, style, name)
{
	if (num_fields < 1)
	{
		throw std::runtime_error("Can't have less than one field in the vector");
	}

	this->m_sizer = new wxBoxSizer(orient);

	wxSpinCtrlDouble* spndbl;
	for (int i = 0; i < num_fields; i++)
	{
		spndbl = new wxSpinCtrlDouble(this, wxID_ANY);
		spndbl->Bind(wxEVT_SPINCTRLDOUBLE, &VectorCtrl::evt_OnValuesChanged, this);

		this->m_sizer->Add(spndbl, wxEXPAND | wxALL);
		this->m_spndbl_fields.push_back(spndbl);
	}

	this->SetSizer(this->m_sizer);
	this->Centre(wxBOTH);
	this->Show();
}

std::vector<int> VectorCtrl::GetDigits()
{
	std::vector<int> results;
	for (auto it = this->m_spndbl_fields.begin(); it != this->m_spndbl_fields.end(); it++)
	{
		results.push_back((int)(*it)->GetDigits());
	}
	return results;
}

double VectorCtrl::GetIncrement()
{
	return this->m_spndbl_fields.at(0)->GetIncrement();
}

double VectorCtrl::GetMax()
{
	return this->m_spndbl_fields.at(0)->GetMax();
}

double VectorCtrl::GetMin()
{
	return this->m_spndbl_fields.at(0)->GetMin();
}

std::vector<double> VectorCtrl::GetValues()
{
	std::vector<double> results;
	for (auto it = this->m_spndbl_fields.begin(); it != this->m_spndbl_fields.end(); it++)
	{
		results.push_back((*it)->GetValue());
	}
	return results;
}

void VectorCtrl::SetDigits(std::vector<int> digits)
{
	if (digits.size() == this->m_spndbl_fields.size())
	{
		for (int i = 0; i < (int)digits.size(); i++)
		{
			this->m_spndbl_fields.at(i)->SetDigits((unsigned int)digits.at(i));
		}
	}
	else
	{
		throw std::runtime_error("Length of \"digits\" must equal number of fields (" + std::to_string(this->m_spndbl_fields.size()) + ")");
	}
}

void VectorCtrl::SetDigits(int digits)
{
	for (auto it = this->m_spndbl_fields.begin(); it != this->m_spndbl_fields.end(); it++)
	{
		(*it)->SetDigits((unsigned int)digits);
	}
}

void VectorCtrl::SetIncrement(double inc)
{
	for (auto it = this->m_spndbl_fields.begin(); it != this->m_spndbl_fields.end(); it++)
	{
		(*it)->SetIncrement(inc);
	}
}

void VectorCtrl::SetRange(double minVal, double maxVal)
{
	for (auto it = this->m_spndbl_fields.begin(); it != this->m_spndbl_fields.end(); it++)
	{
		(*it)->SetRange(minVal, maxVal);
	}
}

void VectorCtrl::SetValues(std::vector<double> values)
{
	for (int i = 0; i < (int)this->m_spndbl_fields.size(); i++)
	{
		this->m_spndbl_fields.at(i)->SetValue(values.at(i));
	}
}

void VectorCtrl::SetValues(double value)
{
	for (auto it = this->m_spndbl_fields.begin(); it != this->m_spndbl_fields.end(); it++)
	{
		(*it)->SetValue(value);
	}
}

VectorCtrlEvent::VectorCtrlEvent(wxEventType eventType, int winid, std::vector<double> values) : wxEvent(winid, eventType)
{
	this->m_values = values;
}

wxEvent* VectorCtrlEvent::Clone() const
{
	return new VectorCtrlEvent(*this);
}

std::vector<double> VectorCtrlEvent::GetValues()
{
	return this->m_values;
}

double VectorCtrlEvent::GetValue(int index)
{
	return this->m_values.at(index);
}
