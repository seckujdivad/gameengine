#include "RevisableResource.h"

int REVISABLERESOURCE_COUNTER = 0; //global counter that ensures that all revision indices are unique

void RevisableResource::IncrementRevisionIndex()
{
	this->m_revision_index = REVISABLERESOURCE_COUNTER++;
}

RevisableResource::RevisableResource() : m_revision_index(REVISABLERESOURCE_COUNTER++)
{
}

int RevisableResource::GetRevisionIndex() const
{
	return this->m_revision_index;
}
