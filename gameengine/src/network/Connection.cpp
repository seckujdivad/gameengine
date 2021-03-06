#include "Connection.h"

#include <stdexcept>

#include <asio/error.hpp>

#include "ConnectionTarget.h"
#include "../generic/ThreadNamer.h"

constexpr char PACKET_DELIMITER = '\n';

void Connection::Listener()
{
	NameThread(L"Socket Listener");

	while (this->m_continue_running.load())
	{
		std::optional<std::vector<char>> header = this->ReadBytes(1);
		if (!header.has_value())
		{
			this->m_continue_running.store(false);
		}
		else if (header.value().size() == 1)
		{
			std::optional<std::vector<char>> body = this->ReadBytes(static_cast<std::size_t>(header.value().at(0)));
			if (body.has_value())
			{
				this->BytesReceived(body.value());
			}
			else
			{
				this->m_continue_running.store(false);
			}
		}
		else
		{
			throw std::runtime_error("Header must be one byte");
		}
	}
}

std::optional<std::vector<char>> Connection::ReadBytes(std::size_t bytes)
{
	if (bytes == 0)
	{
		throw std::invalid_argument("Can't read 0 bytes");
	}

	asio::streambuf buffer;

	asio::error_code error_code;
	std::size_t bytes_read = asio::read(this->m_asio_socket, buffer, asio::transfer_exactly(bytes), error_code);

	if (error_code.value() == asio::error::eof)
	{
		return std::optional<std::vector<char>>();
	}
	else if (error_code.value() != 0)
	{
		throw std::runtime_error("Socket error: " + error_code.message());
	}
	else if (bytes_read != bytes)
	{
		throw std::runtime_error("Less bytes were read than requested");
	}
	else
	{
		const char* bytes_raw = asio::buffer_cast<const char*>(buffer.data());
		return std::vector<char>(bytes_raw, bytes_raw + bytes_read);
	}
}

void Connection::SendBytes(std::vector<char> bytes)
{
	this->m_asio_socket.write_some(asio::buffer(bytes));
}

void Connection::SendBytes(const char* bytes, std::size_t num_bytes)
{
	this->SendBytes(std::vector<char>(bytes, bytes + num_bytes));
}

Connection::Connection(std::string address, unsigned short port) : Connection(ConnectionTarget(address, port))
{
}

Connection::Connection(ConnectionTarget target) : m_asio_socket(this->m_asio_service), m_continue_running(true)
{
	this->m_asio_socket.connect(asio::ip::tcp::endpoint(asio::ip::address::from_string(target.address), target.port));
	this->m_continue_running.store(true);
}

Connection::~Connection()
{
	this->m_continue_running.store(false);
	this->m_asio_socket.shutdown(asio::socket_base::shutdown_both);

	if (this->m_listener.has_value())
	{
		this->m_listener.value().join();
	}

	this->m_asio_socket.close();
}

void Connection::StartListening()
{
	this->m_listener = std::thread(&Connection::Listener, this);
}

bool Connection::IsConnected() const
{
	return this->m_continue_running.load();
}