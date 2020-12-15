#pragma once

#include <vector>
#include <functional>

template<typename T>
inline bool FilterVectorInPlace(std::vector<T>& items, std::function<bool(const T&)> filter)
{
	bool items_modified = false;

	for (auto it = items.rbegin(); it != items.rend();) //do not increment the iterator here so that it can remain valid even after erase is called
	{
		if (filter(*it))
		{
			++it;
		}
		else
		{
			auto it_copy = it;
			++it;
			items.erase(it_copy.base()); //this invalidates it_copy, so it needs to be incremented before erase to remain valid

			items_modified = true;
		}
	}

	return items_modified;
}

template<typename T>
inline bool FilterVectorInPlace(std::vector<T>& items, std::function<bool(T)> filter)
{
	std::function<bool(const T&)> filter_wrapper = [filter](const T& item)
	{
		return filter(item);
	};
	return FilterVectorInPlace(items, filter_wrapper);
}

template<typename T>
inline std::vector<T> FilterVector(std::vector<T> items, std::function<bool(const T&)> filter, bool* items_modified = nullptr)
{
	bool items_modified_result = FilterVectorInPlace(items, filter);
	if (items_modified != nullptr)
	{
		&items_modified = items_modified_result;
	}

	return items;
}

template<typename T>
inline void FilterVector(std::vector<T> items, std::function<bool(T)> filter, bool* items_modified = nullptr)
{
	std::function<bool(const T&)> filter_wrapper = [filter](const T& item)
	{
		return filter(item);
	};
	return FilterVector(items, filter_wrapper, items_modified);
}

template<typename T>
inline bool MapVectorInPlace(std::vector<T>& items, std::function<T(const T&)> map)
{
	bool items_modified = false;

	for (T& item : items)
	{
		T result = map(item);
		if (item != result)
		{
			item = result;
			items_modified = true;
		}
	}

	return items_modified;
}

template<typename T>
inline bool MapVectorInPlace(std::vector<T>& items, std::function<T(T)> map)
{
	std::function<T(const T&)> map_wrapper = [map](const T& item)
	{
		return map(item);
	};
	return MapVectorInPlace(items, map_wrapper);
}

template<typename T>
inline std::vector<T> MapVector(std::vector<T> items, std::function<T(const T&)> map, bool* items_modified = nullptr)
{
	bool items_were_modified = MapVectorInPlace(items, map);

	if (items_modified != nullptr)
	{
		&items_modified = items_were_modified;
	}

	return items;
}

template<typename T>
inline std::vector<T> MapVector(std::vector<T> items, std::function<T(T)> map, bool* items_modified = nullptr)
{
	std::function<T(const T&)> map_wrapper = [map](const T& item)
	{
		return map(item);
	};
	return MapVector(items, map_wrapper, items_modified);
}

template<typename ContainerType, typename ResultType>
inline ResultType FoldVector(std::function<ResultType(ResultType previous, const ContainerType& to_fold)> fold, const std::vector<ContainerType>& items, ResultType initial)
{
	for (const ContainerType& item : items)
	{
		initial = fold(initial, item);
	}
	return initial;
}

template<typename ContainerType, typename ResultType>
inline ResultType FoldVector(std::function<ResultType(ResultType previous, ContainerType to_fold)> fold, const std::vector<ContainerType>& items, ResultType initial)
{
	for (ContainerType item : items)
	{
		initial = fold(initial, item);
	}
	return initial;
}