#include "Packet.h"

#include <stdexcept>
#include <string.h>

Packet::Packet(std::vector<unsigned char> bytes)
{
	this->Deserialise(bytes);
}

void Packet::SetType(Type type)
{
	if (type == Type::ConnEstablished)
	{
		this->m_data = ConnEstablished();
	}
	else if (type == Type::ChatMessage)
	{
		this->m_data = ChatMessage();
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

std::vector<unsigned char> Packet::Serialise() const
{
	std::vector<unsigned char> result;
	result.push_back(static_cast<unsigned char>(this->GetType()));

	if (this->GetType() == Type::ConnEstablished)
	{
		unsigned char uid_bytes[4];
		std::memcpy(uid_bytes, reinterpret_cast<const unsigned char*>(&this->GetData<ConnEstablished>().uid), sizeof(unsigned char) * 4);
	}
	else if (this->GetType() == Type::ChatMessage)
	{
		for (unsigned char c : this->GetData<ChatMessage>().message)
		{
			result.push_back(c);
		}
	}
	else
	{
		throw std::invalid_argument("Unknown type: " + std::to_string(static_cast<int>(this->GetType())));
	}

	return result;
}

void Packet::Deserialise(std::vector<unsigned char> bytes)
{
	if (bytes.size() == 0)
	{
		throw std::invalid_argument("No bytes provided");
	}
	else
	{
		this->SetType(static_cast<Type>(bytes.at(0)));

		if (this->GetType() == Type::ConnEstablished)
		{
			if (bytes.size() == 6)
			{
				this->GetData<ConnEstablished>().uid = *reinterpret_cast<int32_t*>(bytes.data() + sizeof(unsigned char));
			}
			else
			{
				throw std::invalid_argument("Unexpected ConnEstablished packet size");
			}
		}
		else if (this->GetType() == Type::ChatMessage)
		{
			if (bytes.size() == 2)
			{
				this->GetData<ChatMessage>().message = "";
			}
			else if (bytes.size() > 2)
			{
				this->GetData<ChatMessage>().message = std::string(bytes.data() + sizeof(unsigned char), bytes.data() + (bytes.size() * sizeof(unsigned char)));
			}
			else
			{
				throw std::invalid_argument("Packet invalid");
			}
		}
		else
		{
			throw std::invalid_argument("Unknown packet header");
		}
	}
}
