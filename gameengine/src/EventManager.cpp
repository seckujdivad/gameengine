#include "EventManager.h"

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

void EventManager::EmitEvent(std::string event_type, std::vector<void*> additional_data, std::vector<std::size_t> sizes)
{
	auto pair = this->m_event_subscriptions.find(event_type);
	if (pair != this->m_event_subscriptions.end())
	{
		std::vector<EventSubscription> subscriptions = pair->second;
		for (auto it = subscriptions.begin(); it != subscriptions.end(); it++)
		{
			it->function(additional_data, sizes);
		}
	}
}
