#pragma once

#include <type_traits>

#include "Event.h"

using EventUID = std::size_t;

extern EventUID EVENT_UID_COUNTER;

template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
inline EventUID GetDerivedEventUID()
{
	static EventUID uid = EVENT_UID_COUNTER += 1;
	/*
	* Static local variables are instantiated when the function is first called.
	* However, the template means that for each type, a different function (and a new static local variable)
	* exists. Therefore, this number can be used as a unique identifier for the template arguments.
	* Since only the DerivedEvent is in the template arguments, this means that the UID refers to the
	* DerivedEvent type.
	*/
	return uid;
}