#pragma once

#include <unordered_map>
#include <tuple>

template<typename Left, typename Right>
class TwoWayUnorderedMap
{
public:
	using Key = std::size_t;

	class Iterator
	{
	public:
		using WrappedIterator = std::unordered_map<Key, std::tuple<Left, Right>>::iterator;

	private:
		WrappedIterator m_wrapped;

	public:
		inline Iterator(WrappedIterator wrapped) : m_wrapped(wrapped)
		{

		}

		inline Iterator& operator++()
		{
			++this->m_wrapped;
			return *this;
		}

		inline const std::tuple<Left, Right>& operator*() const
		{
			return this->m_wrapped->second;
		}
	};

private:
	Key m_key_count = 0;

	std::unordered_map<Left, Key> m_left_to_key;
	std::unordered_map<Right, Key> m_right_to_key;

	std::unordered_map<Key, std::tuple<Left, Right>> m_key_to_both;

	inline void RemoveByKey(Key key)
	{
		{
			const auto& [left, right] = this->m_key_to_both.at(key);
			this->m_left_to_key.erase(left);
			this->m_right_to_key.erase(right);
		}
		this->m_key_to_both.erase(key);
	}

public:
	inline void Add(Left left, Right right)
	{
		Key key = this->m_key_count++;

		this->m_left_to_key.insert(std::pair(left, key));
		this->m_right_to_key.insert(std::pair(right, key));
		this->m_key_to_both.insert(std::pair(key, std::tuple(left, right)));
	}

	inline bool LeftHas(Left value) const
	{
		return this->m_left_to_key.count(value) != 0;
	}

	inline void LeftRemove(Left value)
	{
		this->RemoveByKey(this->m_left_to_key.at(value));
	}

	inline bool RightHas(Right value) const
	{
		return this->m_right_to_key.count(value) != 0;
	}

	inline void RightRemove(Right value)
	{
		this->RemoveByKey(this->m_right_to_key.at(value));
	}

	//to form a sequence
	inline Iterator begin()
	{
		return Iterator(this->m_key_to_both.begin());
	}

	inline Iterator end()
	{
		return Iterator(this->m_key_to_both.end());
	}
};