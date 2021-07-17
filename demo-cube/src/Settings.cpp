#include "Settings.h"

#include <stdexcept>

std::tuple<std::optional<std::string>, ConnectionTarget> GetConnectionTarget(const nlohmann::json& item)
{
	if (item.is_object())
	{
		std::optional<std::string> name;
		if (item["name"].is_string())
		{
			name = item["name"].get<std::string>();
		}

		if (item["address"].is_string() && item["port"].is_number_integer())
		{
			ConnectionTarget target = ConnectionTarget(item["address"].get<std::string>(), item["port"].get<unsigned short>());
			return std::tuple(name, target);
		}
		else
		{
			throw std::runtime_error("\"address\" must be a string and \"port\" must be an integer - both are required");
		}
	}
	else
	{
		throw std::runtime_error("All server elements must be objects");
	}
}
