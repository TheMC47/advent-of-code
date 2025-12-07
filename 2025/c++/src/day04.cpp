#include <array>
#include <format>
#include <iostream>
#include <optional>
#include <ostream>
#include <string>
#include <string_view>
#include <utils/cli.h>
#include <utils/file.h>
#include <vector>

unsigned long solve(const std::vector<std::string> &inputs) {
  auto result = 0ul;
  auto const height = inputs.size();
  auto const width = inputs[0].size();

  auto const hasPaper = [&inputs, height, width](const auto x, const auto y) {
    return x >= 0 && x < height && y >= 0 && y < width && inputs[x][y] == '@';
  };

  auto const deltas = std::to_array<std::pair<int, int>>(
      {{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}});

  for (auto x = 0u; x < height; x++) {
    for (auto y = 0u; y < width; y++) {
      if (!hasPaper(x, y)) {
        continue;
      }
      auto surrounding = 0ul;
      for (const auto &[dx, dy] : deltas) {
        surrounding += hasPaper(x + dx, y + dy);
      }
      result += (surrounding < 4);
    }
  }

  return result;
}

unsigned long solve2(std::vector<std::string> inputs) {
  auto result = 0ul;
  auto const height = inputs.size();
  auto const width = inputs[0].size();

  auto const hasPaper = [&inputs, height, width](const auto x, const auto y) {
    return x >= 0 && x < height && y >= 0 && y < width && inputs[x][y] == '@';
  };

  auto const deltas = std::to_array<std::pair<int, int>>(
      {{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}});

  std::vector<std::pair<int, int>> toRemove{};
  toRemove.reserve(height * width);
  auto pushed = true;

  do {
    toRemove.clear();
    for (auto x = 0u; x < height; x++) {
      for (auto y = 0u; y < width; y++) {
        if (!hasPaper(x, y)) {
          continue;
        }
        auto surrounding = 0ul;
        for (const auto &[dx, dy] : deltas) {
          surrounding += hasPaper(x + dx, y + dy);
          if (surrounding >= 4) {
            break;
          }
        }
        if (surrounding < 4) {
          toRemove.push_back({x, y});
        }
      }
    }
    result += toRemove.size();
    for (const auto &[x, y] : toRemove) {
      inputs[x][y] = '.';
    }
  } while (toRemove.size() > 0);

  return result;
}

int main(int argc, char *argv[]) {
  auto usage = [argv]() {
    std::cout << std::format("Usage: {} <part> (part: 1, 2)", argv[0])
              << std::endl;
    return 1;
  };

  CLIArguments args{argc, argv};

  const auto part = args.getPositional(0);
  const auto runPart1 = !part.has_value() || part.value() == "1";
  const auto runPart2 = !part.has_value() || part.value() == "2";

  const auto filePath = args.getOpt("-f").value_or("inputs/day04.in");
  const auto lines = readFileLines(filePath);

  if (!runPart1 && !runPart2) {
    std::cout << std::format("Unrecognized part: {}", part.value())
              << std::endl;
    return usage();
  }

  if (runPart1) {
    const auto p1 = solve(lines);
    std::cout << "Part1: " << std::endl;
    std::cout << p1 << std::endl;
  }
  if (runPart2) {
    const auto p2 = solve2(lines);
    std::cout << "Part2: " << std::endl;
    std::cout << p2 << std::endl;
  }
  return 0;
}
