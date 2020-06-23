#include "EventEmitter.h"

EventManager* EventEmitter::GetEventManager()
{
	return this->m_evtman;
}

EventEmitter::EventEmitter(EventManager* evtman)
{
	this->m_evtman = evtman;
}

EventEmitter::EventEmitter(const EventEmitter& copyfrom)
{
	this->m_evtman = copyfrom.m_evtman;
}
