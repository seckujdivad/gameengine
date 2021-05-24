#include "Connection.h"

#include <stdexcept>

#include <asio/error.hpp>

//only required for naming the thread, a debugging convenience
#ifdef _WIN32
#include <windows.h>
#endif

constexpr char PACKET_DELIMITER = '\n';

void Connection::Listener()
{
#ifdef _WIN32
	//https://docs.microsoft.com/en-us/visualstudio/debugger/how-to-set-a-thread-name-in-native-code?view=vs-2019
	SetThreadDescription(GetCurrentThread(), L"Socket listener");
#endif

	while (this->m_continue_running.load())
	{
		asio::streambuf buffer;
		
		asio::error_code error_code;
		std::size_t bytes_read = asio::read_until(this->m_asio_socket, buffer, PACKET_DELIMITER, error_code);

		if (error_code.value() == asio::error::eof)
		{
			this->m_continue_running.store(false);
		}
		else if (error_code.value() != 0)
		{
			throw std::runtime_error("Socket error: " + error_code.message());
		}
		else if (bytes_read == 0)
		{
			throw std::runtime_error("Socket error: " + error_code.message());
		}
		else if (bytes_read == 1)
		{
			this->BytesReceived(std::vector<char>());
		}
		else
		{
			const char* bytes_raw = asio::buffer_cast<const char*>(buffer.data());
			this->BytesReceived(std::vector<char>(bytes_raw, bytes_raw + bytes_read - 1));
		}
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

Connection::Connection(std::string address, unsigned short port) : m_asio_socket(this->m_asio_service), m_continue_running(true)
{
	this->m_asio_socket.connect(asio::ip::tcp::endpoint(asio::ip::address::from_string(address), port));

	this->m_listener = std::thread(&Connection::Listener, this);
}

Connection::~Connection()
{
	this->m_continue_running.store(false);
	this->m_asio_socket.shutdown(asio::socket_base::shutdown_both);
	this->m_listener.join();
	this->m_asio_socket.close();
}
