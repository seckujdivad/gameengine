#pragma once

#include <string>

#include "EventManager.h"

class EventEmitter
{
private:
	EventManager* m_evtman;

protected:
	void EmitEvent(std::string type, nlohmann::json data = nlohmann::json());
	std::string GetClassName();

public:
	EventEmitter(EventManager* evtman);
	EventEmitter(const EventEmitter& copyfrom);

	virtual std::string GetIdentifier() = 0;

	EventManager* GetEventManager();
};