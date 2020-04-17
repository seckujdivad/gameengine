#include <wx/wxprec.h>
#include "Pane.h"

Pane::Pane(PaneHost* parent) : wxPanel(parent)
{
	this->m_parent = parent;
}

Pane::~Pane()
{
}

std::string Pane::GetDisplayName()
{
	return "Pane";
}
