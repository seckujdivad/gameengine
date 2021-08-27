#pragma once

#include <variant>
#include <string>
#include <vector>
#include <array>

#include <stdint.h>

#include <glm/glm.hpp>

#include "serialiser/Field.h"
#include "ModelVectorProperty.h"

/*
* To add a new Packet type:
* * Add to Packet::Type
* * Add struct to Packet
* * Add to struct to variant m_data
* * Add layout to Packet::GetFields
* * Add to Packet::SetType
* * Add to Packet::Deserialise
* * Add to [RECEIVABLE|SENDABLE]_PACKET_TYPES
*/

class Packet
{
public: //type definitions
	enum class Type
	{
		ConnEstablished,
		ClientChatMessage,
		ServerChatMessage,
		SetClientName,
		SetScene,
		SetModelVectorProperty
	};

	struct PacketInner {};

	struct ConnEstablished : public PacketInner
	{
		constexpr ConnEstablished(std::int32_t uid = -1) : uid(uid) {};

		std::int32_t uid;
	};

	struct ClientChatMessage : public PacketInner
	{
		inline ClientChatMessage(std::string message = "") : message(message) {};

		std::string message;
	};

	struct ServerChatMessage : public PacketInner
	{
		inline ServerChatMessage(std::string name = "", std::string message = "") : name(name), message(message) {};

		std::string name;
		std::string message;
	};

	struct SetClientName : public PacketInner
	{
		inline SetClientName(std::string name = "") : name(name) {};

		std::string name;
	};

	struct SetScene : public PacketInner
	{
		inline SetScene(std::string root = "", std::string file = "") : root(root), file(file) {};

		std::string root;
		std::string file;
	};

	struct SetModelVectorProperty : public PacketInner
	{
		inline SetModelVectorProperty(std::string name = "", ModelVectorProperty property = ModelVectorProperty::Position, glm::dvec3 vector = glm::dvec3(0.0)) : name(name), property(property), vector(vector) {};

		std::string name;
		ModelVectorProperty property;
		glm::dvec3 vector;
	};

private:
	std::variant<ConnEstablished, ClientChatMessage, ServerChatMessage, SetClientName, SetScene, SetModelVectorProperty> m_data;

	static std::vector<Serialiser::Field> GetFields(Type type);
	std::vector<Serialiser::Field> GetFields() const;

public:
	template<typename T, typename = std::enable_if_t<std::is_base_of_v<PacketInner, T>>>
	inline Packet(const T& data) : m_data(data) {};
	template<typename T, typename = std::enable_if_t<std::is_base_of_v<PacketInner, T>>>
	inline Packet(T&& data) : m_data(data) {};
	Packet(std::vector<char> bytes);

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

	std::vector<char> Serialise() const;
	void Deserialise(std::vector<char> bytes);
};

constexpr std::array<Packet::Type, 4> RECEIVABLE_PACKET_TYPES = {
	Packet::Type::ConnEstablished,
	Packet::Type::ServerChatMessage,
	Packet::Type::SetScene,
	Packet::Type::SetModelVectorProperty
};

constexpr std::array<Packet::Type, 2> SENDABLE_PACKET_TYPES = {
	Packet::Type::ClientChatMessage,
	Packet::Type::SetClientName
};