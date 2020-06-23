#pragma once

#include <string>
#include <vector>

#include "EventManager.h"

class EventEmitter
{
private:
	EventManager* m_evtman;

protected:
	EventManager* GetEventManager();
	void EmitEvent(std::string type, nlohmann::json data = nlohmann::json());

	std::string GetClassName();

public:
	EventEmitter(EventManager* evtman);
	EventEmitter(const EventEmitter& copyfrom);

	virtual std::string GetIdentifier() = 0;
};