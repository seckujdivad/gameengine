#include "EventManager.h"

#include <stdexcept>

EventManager::EventManager()
{
}

void EventManager::SubscribeToEvent(EventSubscription subscription)
{
	if (this->m_event_subscriptions.find(subscription.type) == this->m_event_subscriptions.end())
	{
		this->m_event_subscriptions.insert({ subscription.type, std::vector<EventSubscription>({subscription}) });
	}
	else
	{
		this->m_event_subscriptions.at(subscription.type).push_back(subscription);
	}
}

void EventManager::EmitEvent(Event evt)
{
	auto pair = this->m_event_subscriptions.find(evt.type);
	if (pair != this->m_event_subscriptions.end())
	{
		std::vector<EventSubscription> subscriptions = pair->second;
		for (auto it = subscriptions.begin(); it != subscriptions.end(); it++)
		{
			it->function(evt);
		}
	}
}
