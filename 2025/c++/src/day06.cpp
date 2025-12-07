#include <algorithm>
#include <cassert>
#include <format>
#include <optional>
#include <print>
#include <ranges>
#include <string>
#include <string_view>
#include <utils/cli.h>
#include <utils/file.h>
#include <vector>

unsigned long parse1(std::string_view line, unsigned from, unsigned to) {
  auto result = 0ul;
  for (int i = from; i < to; i++) {
    if (line[i] != ' ') {
      result *= 10;
      result += line[i] - '0';
    }
  }
  return result;
}

unsigned long solve1(const std::vector<std::string> &inputs) {
  auto columnStart = 0ul;
  auto result = 0ul;

  const auto operations = inputs[inputs.size() - 1];
  const auto lineLength = operations.size();
  const auto numbers = inputs | std::ranges::views::take(inputs.size() - 1);

  while (columnStart < lineLength) {
    auto columnEnd = columnStart + 1;
    while (columnEnd < lineLength &&
           std::ranges::any_of(
               inputs,
               [&columnEnd](auto input) { return input[columnEnd] != ' '; })) {
      columnEnd++;
    }
    const auto operation = operations[columnStart];
    assert(operation == '*' || operation == '+');
    unsigned long local;
    if (operation == '*') {
      local = 1;
      for (const auto &line : numbers) {
        local *= parse1(line, columnStart, columnEnd);
      }
    } else {
      local = 0;
      for (const auto &line : numbers) {
        local += parse1(line, columnStart, columnEnd);
      }
    }
    result += local;
    columnStart = columnEnd + 1;
  }

  return result;
}

unsigned long solve2(const std::vector<std::string> &inputs) {
  auto columnStart = 0ul;
  auto result = 0ul;

  const auto operations = inputs[inputs.size() - 1];
  const auto lineLength = operations.size();
  const auto numbers = inputs | std::ranges::views::take(inputs.size() - 1);

  auto const parse2 = [&numbers](unsigned colIdx) {
    auto result = 0ul;
    for (const auto &line : numbers) {
      if (line[colIdx] != ' ') {
        result *= 10;
        result += line[colIdx] - '0';
      }
    }
    return result;
  };

  while (columnStart < lineLength) {
    auto columnEnd = columnStart + 1;
    while (columnEnd < lineLength &&
           std::ranges::any_of(
               inputs,
               [&columnEnd](auto input) { return input[columnEnd] != ' '; })) {
      columnEnd++;
    }
    const auto operation = operations[columnStart];
    assert(operation == '*' || operation == '+');
    unsigned long local;
    if (operation == '*') {
      local = 1;
      for (int i = columnStart; i < columnEnd; i++) {
        local *= parse2(i);
      }
    } else {
      local = 0;
      for (int i = columnStart; i < columnEnd; i++) {
        local += parse2(i);
      }
    }
    result += local;
    columnStart = columnEnd + 1;
  }

  return result;
}

int main(int argc, char *argv[]) {
  auto usage = [argv]() {
    std::println("Usage: {} <part> (part: 1, 2)", argv[0]);
    return 1;
  };

  CLIArguments args{argc, argv};

  const auto part = args.getPositional(0);
  const auto runPart1 = !part.has_value() || part.value() == "1";
  const auto runPart2 = !part.has_value() || part.value() == "2";

  const auto filePath = args.getOpt("-f").value_or("inputs/day06.in");
  const auto lines = readFileLines(filePath);

  if (!runPart1 && !runPart2) {
    std::println("Unrecognized part: {}", part.value());
    return usage();
  }

  if (runPart1) {
    std::println("Part1:\n{}", solve1(lines));
  }
  if (runPart2) {
    std::println("Part1:\n{}", solve2(lines));
  }
  return 0;
}
