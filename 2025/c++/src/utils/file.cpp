#include "file.h"
#include <fstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

std::string readFile(std::string_view path) {
  std::ifstream file{std::string(path)};
  if (!file.is_open()) {
    throw std::runtime_error("Could not open file");
  }
  return std::string{std::istreambuf_iterator<char>(file),
                     std::istreambuf_iterator<char>()};
}

std::vector<std::string> readFileLines(std::string_view path) {
  std::ifstream file{std::string(path)};
  if (!file.is_open()) {
    throw std::runtime_error("Could not open file");
  }

  std::vector<std::string> lines;
  std::string line;
  while (std::getline(file, line)) {
    lines.push_back(line);
  }
  return lines;
}
