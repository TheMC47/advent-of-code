#include <cassert>
#include <cstdlib>
#include <format>
#include <iostream>
#include <numeric>
#include <optional>
#include <print>
#include <ranges>
#include <string>
#include <string_view>
#include <utils/cli.h>
#include <utils/file.h>
#include <vector>

void wait() {
  std::string name;
  std::getline(std::cin, name);
}

void debug(std::vector<std::string> &inputs) {
  std::system("clear");
  for (const auto &line : inputs) {
    std::println("{}", line);
  }

  wait();
}

std::pair<unsigned long long, unsigned long long>
solve(std::vector<std::string> inputs, const bool shouldDebug) {
  std::vector<unsigned long long> beams(inputs[0].size(), 0ul);
  auto p1 = 0ull;
  for (auto i = 0ul; i < inputs[0].size(); i++) {
    if (inputs[0][i] == 'S') {
      beams[i] = 1;
      inputs[1][i] = '|';
      break;
    }
  }
  if (shouldDebug) {
    debug(inputs);
  }

  for (auto lineIdx = 1; lineIdx < inputs.size(); lineIdx++) {
    for (auto i = 0ul; i < inputs[lineIdx].size(); i++) {
      switch (inputs[lineIdx][i]) {
      case '^': {
        p1++;
        inputs[lineIdx][i - 1] = '|';
        inputs[lineIdx][i + 1] = '|';
        beams[i - 1] += beams[i];
        beams[i + 1] += beams[i];
        beams[i] = 0;
        break;
      }
      case '.': {
        inputs[lineIdx][i] = inputs[lineIdx - 1][i] == '|' ? '|' : '.';
        break;
      }
      }
    }

    if (shouldDebug) {
      debug(inputs);
    }
  }

  return {p1, std::accumulate(beams.begin(), beams.end(), 0ull)};
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
  const auto d = args.hasFlag("-d");

  const auto filePath = args.getOpt("-f").value_or("inputs/day07.in");
  const auto lines = readFileLines(filePath);

  if (!runPart1 && !runPart2) {
    std::println("Unrecognized part: {}", part.value());
    return usage();
  }
  const auto [p1, p2] = solve(lines, d);

  if (runPart1) {
    std::println("Part1:\n{}", p1);
  }
  if (runPart2) {
    std::println("Part1:\n{}", p2);
  }
  return 0;
}
