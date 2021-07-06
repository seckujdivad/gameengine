#pragma once

#include <unordered_map>
#include <functional>
#include <type_traits>
#include <vector>
#include <memory>
#include <stdexcept>
#include <any>

#include "Event.h"
#include "EventUID.h"

class EventHandler
{
public:
	using BoundFunctionUID = std::size_t;

private:
	BoundFunctionUID m_bound_function_counter;

	std::unordered_map<EventUID, std::unordered_map<BoundFunctionUID, std::any>> m_bound_functions; //the any is always EventCallback<DerivedEvent>

public:
	EventHandler();

	template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
	inline BoundFunctionUID BindToEvent(EventCallback<DerivedEvent> callback)
	{
		EventUID derived_uid = GetDerivedEventUID<DerivedEvent>();
		BoundFunctionUID uid = this->m_bound_function_counter += 1;
		
		if (this->m_bound_functions.count(derived_uid) == 0)
		{
			this->m_bound_functions.emplace(derived_uid, std::unordered_map<BoundFunctionUID, std::any>());
		}
		this->m_bound_functions.at(derived_uid).emplace(uid, callback);

		return uid;
	}

	template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
	inline void BroadcastEvent(DerivedEvent event) const
	{
		if (this->m_bound_functions.count(GetDerivedEventUID<DerivedEvent>()) != 0)
		{
			for (const auto& [uid, function] : this->m_bound_functions.at(GetDerivedEventUID<DerivedEvent>()))
			{
				std::any_cast<EventCallback<DerivedEvent>>(function)(event);
			}
		}
	}

	template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
	inline void RemoveBoundFunction(BoundFunctionUID uid)
	{
		EventUID derived_uid = GetDerivedEventUID<DerivedEvent>();
		if (this->m_bound_functions.count(derived_uid) != 0)
		{
			if (this->m_bound_functions.at(derived_uid).count(uid) == 0)
			{
				throw std::invalid_argument("This UID has not been given");
			}
			else
			{
				this->m_bound_functions.at(derived_uid).erase(uid);
			}
		}
		else
		{
			throw std::invalid_argument("No functions are bound for this event type");
		}
	}
};