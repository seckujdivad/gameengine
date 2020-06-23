#include "EventEmitter.h"

EventManager* EventEmitter::GetEventManager()
{
	return this->m_evtman;
}

void EventEmitter::EmitEvent(std::string type, nlohmann::json data)
{
	Event evt;
	evt.type = type;
	evt.emitter_object_name = this->GetIdentifier();
	evt.emitter_object_type = this->GetClassName();
	evt.data = data;

	this->GetEventManager()->EmitEvent(evt);
}

std::string EventEmitter::GetClassName()
{
	std::string raw_name = typeid(*this).name();
	if (raw_name.substr(0, 6) == "class ") //handle implementation specific behaviour
	{
		return raw_name.substr(6);
	}
	else
	{
		return raw_name.substr(std::to_string(raw_name.length() - 2).length());
	}
}

EventEmitter::EventEmitter(EventManager* evtman)
{
	this->m_evtman = evtman;
}

EventEmitter::EventEmitter(const EventEmitter& copyfrom)
{
	this->m_evtman = copyfrom.m_evtman;
}
