#include "Connection.h"

#include <stdexcept>

#include <asio/error.hpp>

void Connection::Listener()
{
	while (this->m_continue_running.load())
	{
		asio::streambuf buffer;
		
		asio::error_code error_code;
		std::size_t bytes_read = asio::read_until(this->m_asio_socket, buffer, '\n', error_code);

		if (error_code.value() == asio::error::eof)
		{
			this->m_continue_running.store(false);
		}
		else if (error_code.value() != 0)
		{
			throw std::runtime_error("Socket error: " + error_code.message());
		}

		std::string s = asio::buffer_cast<const char*>(buffer.data());
	}
}

void Connection::WriteString(std::string to_send)
{
	this->m_asio_socket.write_some(asio::buffer(to_send));
}

Connection::Connection(std::string address, unsigned short port) : m_asio_socket(this->m_asio_service), m_continue_running(true)
{
	this->m_asio_socket.connect(asio::ip::tcp::endpoint(asio::ip::address::from_string(address), port));

	this->m_listener = std::thread(&Connection::Listener, this);
	
	this->WriteString("hello world");
}

Connection::~Connection()
{
	this->m_continue_running.store(false);
	this->m_asio_socket.shutdown(asio::socket_base::shutdown_both);
	this->m_listener.join();
	this->m_asio_socket.close();
}
