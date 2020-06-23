#pragma once

#include <functional>
#include <string>
#include <vector>
#include <map>
#include <stdexcept>

struct EventSubscription
{
	std::string type;
	std::function<void(std::vector<void*>, std::vector<std::size_t>)> function;
};

class EventManager
{
private:
	std::map<std::string, std::vector<EventSubscription>> m_event_subscriptions;

public:
	EventManager();

	void SubscribeToEvent(EventSubscription subscription);
	void EmitEvent(std::string event_type, std::vector<void*> additional_data = {}, std::vector<std::size_t> sizes = {});
};