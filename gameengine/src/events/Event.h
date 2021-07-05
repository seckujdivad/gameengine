#pragma once

#include <functional>

class Event
{
};

template<class DerivedEvent, typename = std::enable_if_t<std::is_base_of_v<Event, DerivedEvent>>>
using EventCallback = std::function<void(const DerivedEvent&)>; //return false to deregister the event