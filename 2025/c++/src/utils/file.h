#pragma once
#include <string>
#include <string_view>
#include <vector>

std::string readFile(std::string_view path);

std::vector<std::string> readFileLines(std::string_view path);
