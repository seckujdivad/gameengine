#pragma once

#include <vector>
#include <memory>
#include <string>

#include "../../scene/model/geometry/Patch.h"

std::vector<std::shared_ptr<Patch>> PatchesFromBPT(std::string path);