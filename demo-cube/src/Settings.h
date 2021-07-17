#pragma once

#include <tuple>
#include <string>
#include <optional>

#include <nlohmann/json.hpp>

#include "network/ConnectionTarget.h"

std::tuple<std::optional<std::string>, ConnectionTarget> GetConnectionTarget(const nlohmann::json& item);