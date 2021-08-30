#pragma once

#include <ctime>

template<typename T>
constexpr T GetTimeBetweenClocks(std::clock_t start, std::clock_t end)
{
	return T(end - start) / T(CLOCKS_PER_SEC);
}

template<typename T>
struct MotionDescriptor
{
	inline MotionDescriptor(T v, T a) : v(v), a(a), set_time(std::clock()) {}

	T v;
	T a;

	std::clock_t set_time;

	template<typename U>
	constexpr T GetDisplacement(U elapsed) const
	{
		return (this->v * T(elapsed)) + (T(U(0.5) * elapsed * elapsed) * this->a);
	}

	constexpr T GetCurrentDisplacement(std::clock_t current_time)
	{
		return this->GetDisplacement<double>(GetTimeBetweenClocks<double>(this->set_time, current_time));
	}
};