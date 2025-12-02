#include <format>
#include <iostream>
#include <optional>
#include <ostream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
#include <utils/cli.h>
#include <utils/file.h>
#include <vector>

template <typename T> int sgn(T val) { return (T(0) < val) - (val < T(0)); }

inline int wrap100(int x) {
  x %= 100;
  return x < 0 ? x += 100 : x;
}

std::pair<std::string, std::string>
solve(const std::vector<std::string> &lines) {
  int dial = 50;
  int password1 = 0;
  int password2 = 0;
  for (const auto &line : lines) {
    int steps;
    auto result =
        std::from_chars(line.data() + 1, line.data() + line.size(), steps);
    if (result.ec != std::errc()) {
      std::cerr << std::make_error_code(result.ec).message() << std::endl;
      throw std::runtime_error(
          std::format("Could not read number at line {}", line));
    }

    const int oldDial = dial;

    if (line[0] == 'L') {
      dial -= steps;
    } else if (line[0] == 'R') {
      dial += steps;
    } else {
      throw std::runtime_error(std::format("Unexpected line {}", line));
    }

    // Handle full rotations, and then check if we land on zero or we have a non
    // full rotation
    password2 += std::abs(dial) / 100;
    password2 += (dial == 0 || (oldDial != 0 && sgn(oldDial) != sgn(dial)));

    dial = wrap100(dial);

    password1 += (dial == 0);
  }
  return {std::to_string(password1), std::to_string(password2)};
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

  const auto filePath = args.getOpt("-f").value_or("inputs/day01.in");
  const auto lines = readFileLines(filePath);

  if (!runPart1 && !runPart2) {
    std::cout << std::format("Unrecognized part: {}", part.value())
              << std::endl;
    return usage();
  }

  const auto [p1, p2] = solve(lines);

  if (runPart1) {
    std::cout << "Part1: " << std::endl;
    std::cout << p1 << std::endl;
  }
  if (runPart2) {
    std::cout << "Part2: " << std::endl;
    std::cout << p2 << std::endl;
  }
  return 0;
}
