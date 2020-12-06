#include "VectorCtrl.h"

#include <stdexcept>

#include <wx/gbsizer.h>

wxDEFINE_EVENT(EVT_VCTRCTRL_CHANGED, VectorCtrlEvent);

void VectorCtrl::evt_OnValuesChanged(wxSpinDoubleEvent& evt)
{
	std::vector<double> values = this->GetValues();
	for (double& value : values)
	{
		if ((!this->m_can_be_min && (value == this->GetMin())) || (!this->m_can_be_max && (value == this->GetMax())))
		{
			value = this->GetMin() + ((this->GetMax() - this->GetMin()) * 0.5);
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
		throw std::invalid_argument("Can't have less than one field in the vector");
	}

	if ((config.orient != wxVERTICAL) && (config.orient != wxHORIZONTAL))
	{
		throw std::invalid_argument("Orientation must be either wxVERTICAL or wxHORIZONTAL");
	}

	if (config.max < config.min)
	{
		throw std::invalid_argument("Max cannot be less than min");
	}

	if (config.can_be_max)
	{
		if (config.initial > config.max)
		{
			throw std::invalid_argument("Initial value must be less than or equal to max");
		}
	}
	else
	{
		if (config.initial >= config.max)
		{
			throw std::invalid_argument("Initial value must be less than to max");
		}
	}

	if (config.can_be_min)
	{
		if (config.initial < config.min)
		{
			throw std::invalid_argument("Initial value must be greater than or equal to min");
		}
	}
	else
	{
		if (config.initial <= config.min)
		{
			throw std::invalid_argument("Initial value must be greater than to min");
		}
	}

	this->m_can_be_max = config.can_be_max;
	this->m_can_be_min = config.can_be_min;

	this->m_sizer = new wxGridBagSizer(0, 0);

	for (int i = 0; i < config.num_fields; i++)
	{
		wxSpinCtrlDouble* spndbl = new wxSpinCtrlDouble(
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
		spndbl->SetMinSize(wxSize(45, spndbl->GetMinSize().y)); //45 px seems to be a more reasonable minimum x (displays 2 digits) than the default (displays approx 10 digits)
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
	this->Layout();
}

std::vector<int> VectorCtrl::GetDigits() const
{
	std::vector<int> results;
	for (wxSpinCtrlDouble* spndbl_field : this->m_spndbl_fields)
	{
		results.push_back(static_cast<int>(spndbl_field->GetDigits()));
	}
	return results;
}

double VectorCtrl::GetIncrement() const
{
	return this->m_spndbl_fields.at(0)->GetIncrement();
}

double VectorCtrl::GetMax() const
{
	return this->m_spndbl_fields.at(0)->GetMax();
}

double VectorCtrl::GetMin() const
{
	return this->m_spndbl_fields.at(0)->GetMin();
}

std::vector<double> VectorCtrl::GetValues() const
{
	std::vector<double> results;
	for (wxSpinCtrlDouble* spndbl_field : this->m_spndbl_fields)
	{
		results.push_back(spndbl_field->GetValue());
	}
	return results;
}

void VectorCtrl::SetDigits(std::vector<int> digits)
{
	if (digits.size() == this->m_spndbl_fields.size())
	{
		for (size_t i = 0; i < digits.size(); i++)
		{
			this->m_spndbl_fields.at(i)->SetDigits(static_cast<unsigned int>(digits.at(i)));
		}
	}
	else
	{
		throw std::runtime_error("Length of \"digits\" must equal number of fields (" + std::to_string(this->m_spndbl_fields.size()) + ")");
	}
}

void VectorCtrl::SetDigits(int digits)
{
	for (wxSpinCtrlDouble* spndbl_field : this->m_spndbl_fields)
	{
		spndbl_field->SetDigits(static_cast<unsigned int>(digits));
	}
}

void VectorCtrl::SetIncrement(double inc)
{
	for (wxSpinCtrlDouble* spndbl_field : this->m_spndbl_fields)
	{
		spndbl_field->SetIncrement(inc);
	}
}

void VectorCtrl::SetRange(double minVal, double maxVal)
{
	if (minVal < maxVal)
	{
		for (wxSpinCtrlDouble* spndbl_field : this->m_spndbl_fields)
		{
			spndbl_field->SetRange(minVal, maxVal);
		}
	}
	else
	{
		throw std::runtime_error("minVal must be less than (not equal to or greater than) maxVal");
	}
}

void VectorCtrl::SetValues(std::vector<double> values)
{
	for (int i = 0; i < static_cast<int>(this->m_spndbl_fields.size()); i++)
	{
		this->m_spndbl_fields.at(i)->SetValue(values.at(i));
	}
}

void VectorCtrl::SetValues(double value)
{
	for (wxSpinCtrlDouble* spndbl_field : this->m_spndbl_fields)
	{
		spndbl_field->SetValue(value);
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

VectorCtrlEvent::VectorCtrlEvent(wxEventType eventType, int winid, std::vector<double> values) : wxEvent(winid, eventType), m_values(values)
{
}

wxEvent* VectorCtrlEvent::Clone() const
{
	return new VectorCtrlEvent(*this);
}

std::vector<double> VectorCtrlEvent::GetValues() const
{
	return this->m_values;
}

double VectorCtrlEvent::GetValue(int index) const
{
	return this->m_values.at(index);
}
