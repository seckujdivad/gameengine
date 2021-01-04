#pragma once

class RevisableResource
{
private:
	int m_revision_index; //unique index for the current revision of the data held by the derived class

protected:
	void IncrementRevisionIndex(); //generate a new unique revision index for the data held by the derived class

public:
	RevisableResource();

	int GetRevisionIndex() const; //get the unique revision index of the data held by the derived class
};