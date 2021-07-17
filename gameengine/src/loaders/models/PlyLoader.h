#pragma once

#include <string>
#include <memory>
#include <vector>

class Polygonal;

std::shared_ptr<Polygonal> ModelFromPly(std::string path);
std::shared_ptr<Polygonal> ModelFromPlyText(const std::string& text);
std::shared_ptr<Polygonal> ModelFromPlyText(const std::vector<std::string>& text);