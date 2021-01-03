#pragma once

#include <string>
#include <filesystem>

std::string LoadFile(std::filesystem::path path);
std::string LoadFile(std::string path);
std::string LoadFile(const char* path);