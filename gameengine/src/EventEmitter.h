#pragma once

#include "EventManager.h"

class EventEmitter
{
private:
	EventManager* m_evtman;

protected:
	EventManager* GetEventManager();

public:
	EventEmitter(EventManager* evtman);
	EventEmitter(const EventEmitter& copyfrom);
};