#pragma once

#include "EngineConnection.h"

class wxWindow;

void ProcessOutstandingPackets(EngineConnection& connection, wxWindow* emit_events_from = nullptr);