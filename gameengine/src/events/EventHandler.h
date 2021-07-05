#pragma once

#include <unordered_map>
#include <functional>
#include <type_traits>
#include <vector>
#include <memory>
#include <stdexcept>

#include "Event.h"
#include "EventUID.h"

class EventHandler
{
public:
	using BoundFunctionUID = std::size_t;

private:
	BoundFunctionUID m_bound_function_counter;

	template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
	using FunctionCollection = std::unordered_map<BoundFunctionUID, EventCallback<DerivedEvent>>;

	std::unordered_map<EventUID, std::unique_ptr<int>> m_bound_functions;

	template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
	inline FunctionCollection<DerivedEvent>& GetEventFunctions()
	{
		std::unique_ptr<int>& functions_uncast = this->m_bound_functions.at(GetDerivedEventUID<DerivedEvent>());
		FunctionCollection<DerivedEvent>& functions = *reinterpret_cast<FunctionCollection<DerivedEvent>*>(functions_uncast.get());
		return functions;
	}

	template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
	inline const FunctionCollection<DerivedEvent>& GetEventFunctions() const
	{
		const std::unique_ptr<int>& functions_uncast = this->m_bound_functions.at(GetDerivedEventUID<DerivedEvent>());
		const FunctionCollection<DerivedEvent>& functions = *reinterpret_cast<FunctionCollection<DerivedEvent>*>(functions_uncast.get());
		return functions;
	}

	template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
	inline void InitialiseEventFunctions()
	{
		FunctionCollection<DerivedEvent>* functions = new FunctionCollection<DerivedEvent>();
		this->m_bound_functions.insert(std::pair(GetDerivedEventUID<DerivedEvent>(), std::unique_ptr<int>(reinterpret_cast<int*>(functions))));
	}

	template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
	inline FunctionCollection<DerivedEvent>& ForceGetEventFunctions()
	{
		if (this->m_bound_functions.count(GetDerivedEventUID<DerivedEvent>()) == 0)
		{
			this->InitialiseEventFunctions<DerivedEvent>();
		}
		return this->GetEventFunctions<DerivedEvent>();
	}

	template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
	inline bool DerivedEventHasBoundFunctions() const
	{
		return this->m_bound_functions.count(GetDerivedEventUID<DerivedEvent>()) != 0;
	}

public:
	EventHandler();

	//enforce no copying - the unique ptrs should enforce this anyway, but make it explicit
	EventHandler(const EventHandler&) = delete;
	EventHandler& operator=(const EventHandler&) = delete;

	template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
	inline BoundFunctionUID BindToEvent(EventCallback<DerivedEvent> callback)
	{
		BoundFunctionUID uid = this->m_bound_function_counter += 1;
		this->ForceGetEventFunctions<DerivedEvent>().insert(std::pair(uid, callback));
		return uid;
	}

	template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
	inline void BroadcastEvent(DerivedEvent event) const
	{
		if (this->DerivedEventHasBoundFunctions<DerivedEvent>())
		{
			for (const auto& [uid, function] : this->GetEventFunctions<DerivedEvent>())
			{
				function(event);
			}
		}
	}

	template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
	inline void RemoveBoundFunction(BoundFunctionUID uid)
	{
		if (this->DerivedEventHasBoundFunctions<DerivedEvent>())
		{
			const FunctionCollection<DerivedEvent>& functions = this->GetEventFunctions<DerivedEvent>();
			if (this->m_bound_functions.count(uid) == 0)
			{
				throw std::invalid_argument("This UID has not been given");
			}
			else
			{
				this->m_bound_functions.erase(uid);
			}
		}
		else
		{
			throw std::invalid_argument("No functions are bound for this event type");
		}
	}
};