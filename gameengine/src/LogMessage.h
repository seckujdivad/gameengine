#pragma once

#include <string>

const char GAMEENGINE_LOG_PATH[] = "gameengine_GL.log";

void LogMessage(std::string message, bool show_time = true);
void LogMessage(const char* message, bool show_time = true);