#include "Packet.h"

#include <stdexcept>
#include <string.h>

#include "serialiser/Serialise.h"
#include "serialiser/Deserialise.h"

std::vector<Serialiser::Field> Packet::GetFields(Type type)
{
	if (type == Type::ConnEstablished)
	{
		return std::vector<Serialiser::Field>({
			Serialiser::Field(Serialiser::Type::Int32, offsetof(ConnEstablished, uid))
			});
	}
	else if (type == Type::ClientChatMessage)
	{
		return std::vector<Serialiser::Field>({
			Serialiser::Field(Serialiser::Type::UnlimitedString, offsetof(ClientChatMessage, message))
			});
	}
	else if (type == Type::ServerChatMessage)
	{
		return std::vector<Serialiser::Field>({
			Serialiser::Field(Serialiser::Type::NullTerminatedString, offsetof(ServerChatMessage, name)),
			Serialiser::Field(Serialiser::Type::UnlimitedString, offsetof(ServerChatMessage, message))
			});
	}
	else if (type == Type::SetClientName)
	{
		return std::vector<Serialiser::Field>({
			Serialiser::Field(Serialiser::Type::UnlimitedString, offsetof(SetClientName, name))
			});
	}
	else if (type == Type::SetScene)
	{
		return std::vector<Serialiser::Field>({
			Serialiser::Field(Serialiser::Type::NullTerminatedString, offsetof(SetScene, root)),
			Serialiser::Field(Serialiser::Type::UnlimitedString, offsetof(SetScene, file))
			});
	}
	else
	{
		throw std::invalid_argument("Unknown type: " + std::to_string(static_cast<int>(type)));
	}
}

std::vector<Serialiser::Field> Packet::GetFields() const
{
	return this->GetFields(this->GetType());
}

Packet::Packet(std::vector<char> bytes)
{
	this->Deserialise(bytes);
}

void Packet::SetType(Type type)
{
	if (type == Type::ConnEstablished)
	{
		this->m_data = ConnEstablished();
	}
	else if (type == Type::ClientChatMessage)
	{
		this->m_data = ClientChatMessage();
	}
	else if (type == Type::ServerChatMessage)
	{
		this->m_data = ServerChatMessage();
	}
	else if (type == Type::SetClientName)
	{
		this->m_data = SetClientName();
	}
	else
	{
		throw std::invalid_argument("Unknown type: " + std::to_string(static_cast<int>(type)));
	}
}

Packet::Type Packet::GetType() const
{
	return static_cast<Packet::Type>(this->m_data.index());
}

std::vector<char> Packet::Serialise() const
{
	std::vector<char> result;
	result.push_back(static_cast<char>(this->GetType()));

	std::vector<char> body_bytes;
	std::vector<Serialiser::Field> fields = this->GetFields();
	if (this->GetType() == Type::ConnEstablished)
	{
		body_bytes = Serialiser::Serialise(this->GetData<ConnEstablished>(), fields);
	}
	else if (this->GetType() == Type::ClientChatMessage)
	{
		body_bytes = Serialiser::Serialise(this->GetData<ClientChatMessage>(), fields);
	}
	else if (this->GetType() == Type::ServerChatMessage)
	{
		body_bytes = Serialiser::Serialise(this->GetData<ServerChatMessage>(), fields);
	}
	else if (this->GetType() == Type::SetClientName)
	{
		body_bytes = Serialiser::Serialise(this->GetData<SetClientName>(), fields);
	}
	else
	{
		throw std::invalid_argument("Unknown type: " + std::to_string(static_cast<int>(this->GetType())));
	}

	result.insert(result.end(), body_bytes.begin(), body_bytes.end());

	return result;
}

void Packet::Deserialise(std::vector<char> bytes)
{
	if (bytes.size() == 0)
	{
		throw std::invalid_argument("No bytes provided");
	}
	else
	{
		this->SetType(static_cast<Type>(bytes.at(0)));

		std::vector<Serialiser::Field> fields = this->GetFields();
		if (this->GetType() == Type::ConnEstablished)
		{
			this->m_data = Serialiser::Deserialise<ConnEstablished>(bytes, fields, 1);
		}
		else if (this->GetType() == Type::ClientChatMessage)
		{
			this->m_data = Serialiser::Deserialise<ClientChatMessage>(bytes, fields, 1);
		}
		else if (this->GetType() == Type::ServerChatMessage)
		{
			this->m_data = Serialiser::Deserialise<ServerChatMessage>(bytes, fields, 1);
		}
		else if (this->GetType() == Type::SetClientName)
		{
			this->m_data = Serialiser::Deserialise<SetClientName>(bytes, fields, 1);
		}
		else
		{
			throw std::invalid_argument("Unknown packet header");
		}
	}
}
