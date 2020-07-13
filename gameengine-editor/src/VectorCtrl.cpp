#include "VectorCtrl.h"

wxDEFINE_EVENT(EVT_VCTRCTRL_CHANGED, VectorCtrlEvent);

void VectorCtrl::evt_OnValuesChanged(wxSpinDoubleEvent& evt)
{
	std::vector<double> values = this->GetValues();
	for (int i = 0; i < (int)values.size(); i++)
	{
		if ((!this->m_can_be_min && (values.at(i) == this->GetMin())) || (!this->m_can_be_max && (values.at(i) == this->GetMax())))
		{
			values.at(i) = this->GetMin() + ((this->GetMax() - this->GetMin()) * 0.5);
		}
	}

	VectorCtrlEvent event = VectorCtrlEvent(EVT_VCTRCTRL_CHANGED, this->GetId(), values);
	event.SetEventObject(this);
	this->ProcessWindowEvent(event);

	evt.Skip();
}

VectorCtrl::VectorCtrl(wxWindow* parent, wxWindowID winid, VectorCtrlConfig config, const wxPoint& pos, const wxSize& size, long int style, const wxString& name) : wxPanel(parent, winid, pos, size, style, name)
{
	if (config.num_fields < 1)
	{
		throw std::runtime_error("Can't have less than one field in the vector");
	}

	this->m_can_be_max = config.can_be_max;
	this->m_can_be_min = config.can_be_min;

	this->m_sizer = new wxGridBagSizer(0, 0);

	wxSpinCtrlDouble* spndbl;
	for (int i = 0; i < config.num_fields; i++)
	{
		spndbl = new wxSpinCtrlDouble(
			this,
			wxID_ANY,
			wxEmptyString,
			wxDefaultPosition,
			wxDefaultSize,
			config.style,
			config.min,
			config.max,
			config.initial,
			config.inc
		);

		spndbl->Bind(wxEVT_SPINCTRLDOUBLE, &VectorCtrl::evt_OnValuesChanged, this);

		this->m_sizer->Add(spndbl, wxGBPosition(config.orient == wxVERTICAL ? i : 0, config.orient == wxHORIZONTAL ? i : 0), wxGBSpan(1, 1), wxEXPAND | wxALL);
		this->m_spndbl_fields.push_back(spndbl);

		if (config.orient == wxVERTICAL)
		{
			this->m_sizer->AddGrowableRow(i);
		}
		else if(config.orient == wxHORIZONTAL)
		{
			this->m_sizer->AddGrowableCol(i);
		}
	}

	if (config.orient == wxVERTICAL)
	{
		this->m_sizer->AddGrowableCol(0);
	}
	else if (config.orient == wxHORIZONTAL)
	{
		this->m_sizer->AddGrowableRow(0);
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
	if (minVal < maxVal)
	{
		for (auto it = this->m_spndbl_fields.begin(); it != this->m_spndbl_fields.end(); it++)
		{
			(*it)->SetRange(minVal, maxVal);
		}
	}
	else
	{
		throw std::runtime_error("minVal must be less than (not equal to or greater than) maxVal");
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

void VectorCtrl::SetMax(double max_value)
{
	this->SetRange(this->GetMin(), max_value);
}

void VectorCtrl::SetMin(double min_value)
{
	this->SetRange(min_value, this->GetMax());
}

void VectorCtrl::ValuesCanBeMin(bool can_be_min)
{
	this->m_can_be_min = can_be_min;
}

void VectorCtrl::ValuesCanBeMax(bool can_be_max)
{
	this->m_can_be_max = can_be_max;
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
