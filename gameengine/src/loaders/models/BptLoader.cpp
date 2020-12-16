#include "BptLoader.h"

#include <fstream>

#include "../../generic/SplitOnChar.h"

std::vector<std::shared_ptr<Patch>> PatchesFromBPT(std::string path)
{
	std::vector<std::shared_ptr<Patch>> patches;

	std::ifstream file;
	file.open(path);

	if (file.is_open())
	{
		int target_patches = 0;
		int target_patch_lines = 0;
		int target_patch_line_length = 0;
		int num_patch_line_control_points = 0;

		int num_patch_lines = 0;

		std::vector<std::vector<Patch::ControlPoint>> patch_data;

		int line_num = 0;
		std::string line;
		while (std::getline(file, line))
		{
			if (line_num == 0) //first line contains the number of patches
			{
				target_patches = std::stoi(line);
				patches.reserve(target_patches);
			}
			else
			{
				if ((target_patch_lines == 0) && (target_patch_line_length == 0))
				{
					std::vector<std::string> patch_dimensions = SplitOnChar(line, ' ');
					if (patch_dimensions.size() == 2)
					{
						target_patch_lines = std::stoi(patch_dimensions.at(0)) + 1;
						target_patch_line_length = std::stoi(patch_dimensions.at(1)) + 1;

						if (target_patch_lines < 2)
						{
							throw std::runtime_error("Line " + std::to_string(line_num) + ": number of patch lines must be at least 2");
						}

						if (target_patch_line_length < 2)
						{
							throw std::runtime_error("Line " + std::to_string(line_num) + ": number of values in each patch line must be at least 2");
						}
					}
					else
					{
						throw std::runtime_error("Line " + std::to_string(line_num) + ": two values must be provided when setting the patch dimensions");
					}
				}
				else
				{
					std::vector<std::string> control_point_values = SplitOnChar(line, ' ');
					if (control_point_values.size() == 3)
					{
						glm::dvec3 vertex = glm::dvec3(
							std::stod(control_point_values.at(0)),
							std::stod(control_point_values.at(1)),
							std::stod(control_point_values.at(2))
						);

						glm::dvec2 uv = glm::dvec2(
							(num_patch_line_control_points + 1) / target_patch_line_length,
							(num_patch_lines + 1) / target_patch_lines
						);

						Patch::ControlPoint control_point(vertex, uv);

						if ((num_patch_lines == 0) || (num_patch_line_control_points == target_patch_line_length))
						{
							patch_data.push_back(std::vector({ control_point }));
							num_patch_line_control_points = 1;
							num_patch_lines++;
						}
						else
						{
							patch_data.at(num_patch_lines - 1).push_back(control_point);
							num_patch_line_control_points++;
						}
					}
					else
					{
						throw std::runtime_error("Line " + std::to_string(line_num) + ": three values must be provided for each control point");
					}
				}

				if ((num_patch_lines == target_patch_lines) && (num_patch_line_control_points == target_patch_line_length))
				{
					std::shared_ptr<Patch> patch = std::make_shared<Patch>();
					patch->SetControlPoints(patch_data);
					patches.push_back(patch);

					patch_data.clear();
					target_patch_lines = 0;
					target_patch_line_length = 0;

					num_patch_lines = 0;
					num_patch_line_control_points = 0;
				}
			}

			line_num++;
		}

		if (target_patches != static_cast<int>(patches.size()))
		{
			throw std::runtime_error("BPT file said it would provide " + std::to_string(target_patches) + " patch(es), but it actually provided " + std::to_string(static_cast<int>(patches.size())));
		}
	}
	else
	{
		throw std::invalid_argument("Couldn't open file at \"" + path + "\"");
	}

	return patches;
}
