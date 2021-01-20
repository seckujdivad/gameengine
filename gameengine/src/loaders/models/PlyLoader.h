#pragma once

#include <vector>
#include <string>
#include <memory>

#include "../../scene/model/geometry/Polygonal.h"

std::shared_ptr<Polygonal> ModelFromPly(std::string path);
std::shared_ptr<Polygonal> ModelFromPlyText(std::string text);