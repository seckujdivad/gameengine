#pragma once

#include <functional>
#include <string>
#include <vector>
#include <map>

#include <nlohmann/json.hpp>

struct Event
{
	std::string type;
	std::string emitter_object_type;
	std::string emitter_object_name;
	nlohmann::json data; //these pointers are only guaranteed to be valid during EventSubscription::function
};

struct EventSubscription
{
	std::string type;
	std::function<void(Event)> function;
};

class EventManager
{
private:
	std::map<std::string, std::vector<EventSubscription>> m_event_subscriptions;

public:
	EventManager();

	void SubscribeToEvent(EventSubscription subscription);
	void EmitEvent(Event evt);
};