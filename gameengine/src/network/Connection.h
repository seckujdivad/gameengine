#pragma once

#include <string>
#include <optional>
#include <memory>
#include <thread>
#include <atomic>

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0501
#include <asio.hpp>
#undef _WIN32_WINNT
#else
#include <asio.hpp>
#endif

class Scene;

class Connection
{
private:
	asio::io_service m_asio_service;
	asio::ip::tcp::socket m_asio_socket;

	std::atomic<bool> m_continue_running;
	std::thread m_listener;
	void Listener();

protected:
	void SendBytes(std::vector<unsigned char> bytes);
	void SendBytes(const unsigned char* bytes, std::size_t num_bytes);

	virtual void BytesReceived(std::vector<unsigned char> bytes) = 0;

public:
	Connection(std::string address, unsigned short port);
	Connection(const Connection&) = delete;
	Connection& operator=(const Connection&) = delete;
	Connection(Connection&&) = delete;
	Connection& operator=(Connection&&) = delete;
	~Connection();
};