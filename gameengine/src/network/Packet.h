#pragma once

#include <variant>
#include <string>
#include <vector>

#include <stdint.h>

class Packet
{
public: //type definitions
	enum class Type
	{
		ConnEstablished,
		ChatMessage
	};

	struct PacketInner {};

	struct ConnEstablished : public PacketInner
	{
		constexpr ConnEstablished(std::int32_t uid = -1) : uid(uid) {};

		std::int32_t uid;
	};

	struct ChatMessage : public PacketInner
	{
		inline ChatMessage(std::string message = "") : message(message) {};

		std::string message;
	};

private:
	std::variant<ConnEstablished, ChatMessage> m_data;

public:
	template<typename T, typename = std::enable_if_t<std::is_base_of_v<PacketInner, T>>>
	inline Packet(T data) : m_data(data) {};
	Packet(std::vector<unsigned char> bytes);

	void SetType(Type type);
	Type GetType() const;

	template<typename T, typename = std::enable_if_t<std::is_base_of_v<PacketInner, T>>>
	inline T& GetData()
	{
		return std::get<T>(this->m_data);
	}

	template<typename T, typename = std::enable_if_t<std::is_base_of_v<PacketInner, T>>>
	inline const T& GetData() const
	{
		return std::get<T>(this->m_data);
	}

	std::vector<unsigned char> Serialise() const;
	void Deserialise(std::vector<unsigned char> bytes);
};