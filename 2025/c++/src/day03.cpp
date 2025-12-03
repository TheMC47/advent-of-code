#include <format>
#include <iostream>
#include <optional>
#include <ostream>
#include <ranges>
#include <string>
#include <string_view>
#include <utils/cli.h>
#include <utils/file.h>
#include <vector>

std::pair<unsigned long, unsigned long>
largestFromTo(const std::vector<unsigned long> &line, unsigned long from,
              unsigned long to) {
  auto largest = line[from];
  auto largestIdx = from;
  for (auto i = from + 1; i < to; i++) {
    if (line[i] > largest) {
      largest = line[i];
      largestIdx = i;
    }
  }
  return {largestIdx, largest};
}

unsigned long solve1(const std::vector<std::vector<unsigned long>> &inputs) {
  auto result = 0ul;
  for (const auto &line : inputs) {
    auto [largestIdx, largest] = largestFromTo(line, 0, line.size() - 1);
    auto [_, largest2] = largestFromTo(line, largestIdx, line.size());
    result += largest * 10 + largest2;
  }
  return result;
}

unsigned long solve2(const std::vector<std::vector<unsigned long>> &inputs) {
  auto result = 0ul;
  for (const auto &line : inputs) {
    auto joltage = 0ul;
    auto lastTakenIdx = -1;
    for (const auto i : std::views::iota(0, 12)) {
      auto [nextTakenIdx, largest] =
          largestFromTo(line, lastTakenIdx + 1, line.size() - (12 - i - 1));
      joltage *= 10;
      joltage += largest;
      lastTakenIdx = nextTakenIdx;
    }
    result += joltage;
  }
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

  const auto filePath = args.getOpt("-f").value_or("inputs/day03.in");
  const auto lines = readFileLines(filePath);

  if (!runPart1 && !runPart2) {
    std::cout << std::format("Unrecognized part: {}", part.value())
              << std::endl;
    return usage();
  }

  const auto input =
      lines | std::ranges::views::transform([](std::string line) {
        return line |
               std::ranges::views::transform(
                   [](char c) -> unsigned long { return c - '0'; }) |
               std::ranges::to<std::vector>();
      }) |
      std::ranges::to<std::vector>();

  if (runPart1) {
    const auto p1 = solve1(input);
    std::cout << "Part1: " << std::endl;
    std::cout << p1 << std::endl;
  }
  if (runPart2) {
    const auto p2 = solve2(input);
    std::cout << "Part2: " << std::endl;
    std::cout << p2 << std::endl;
  }
  return 0;
}
