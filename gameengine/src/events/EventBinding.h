#pragma once

#include <vector>
#include <memory>
#include <tuple>

#include "Event.h"
#include "EventHandler.h"

template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
class EventBinding
{
private:
	EventCallback<DerivedEvent> m_callback;

	std::vector<std::tuple<EventHandler::BoundFunctionUID, std::shared_ptr<EventHandler>>> m_handlers_owning;
	std::vector<std::tuple<EventHandler::BoundFunctionUID, EventHandler*>> m_handlers_non_owning;

	inline std::vector<std::tuple<EventHandler::BoundFunctionUID, EventHandler*>> GetHandlers() const
	{
		std::vector<std::tuple<EventHandler::BoundFunctionUID, EventHandler*>> result = this->m_handlers_non_owning;
		for (const auto& [uid, ptr] : this->m_handlers_owning)
		{
			result.emplace_back(uid, ptr.get());
		}
		return result;
	}

public:
	inline EventBinding(EventCallback<DerivedEvent> callback) : m_callback(callback) {}

	EventBinding(const EventBinding&) = delete;
	EventBinding& operator=(const EventBinding&) = delete;

	inline EventBinding(EventBinding&& move_from)
	{
		*this = std::move(move_from);
	}

	inline EventBinding& operator=(EventBinding&& move_from)
	{
		this->UnregisterAll();

		this->m_callback = move_from.m_callback;
		this->m_handlers_non_owning = move_from.m_handlers_non_owning;
		this->m_handlers_owning = move_from.m_handlers_owning;

		move_from.UnregisterAll();
	}

	inline ~EventBinding()
	{
		this->UnregisterAll();
	}

	inline void RegisterHandler(EventHandler* handler)
	{
		this->m_handlers_non_owning.emplace_back(handler->BindToEvent(this->m_callback), handler);
	}

	inline void RegisterHandler(std::shared_ptr<EventHandler> handler)
	{
		this->m_handlers_owning.emplace_back(handler->BindToEvent(this->m_callback), handler);
	}

	inline void UnregisterAll()
	{
		for (const auto& [uid, ptr] : this->GetHandlers())
		{
			ptr->RemoveBoundFunction<DerivedEvent>(uid);
		}
		this->m_handlers_owning.clear();
		this->m_handlers_non_owning.clear();
	}
};