#include <algorithm>
#include <format>
#include <iostream>
#include <optional>
#include <ostream>
#include <ranges>
#include <stdexcept>
#include <string>
#include <string_view>
#include <system_error>
#include <utils/cli.h>
#include <utils/file.h>
#include <vector>

template <typename N> N parse(std::string_view sv) {
  N out;
  const auto result = std::from_chars(sv.data(), sv.data() + sv.size(), out);
  if (result.ec != std::errc()) {
    std::cerr << std::make_error_code(result.ec).message() << std::endl;
    throw std::runtime_error(std::format("Could not read number from {}", sv));
  }
  return out;
}

bool isInvalid1(unsigned long n) {
  const auto asStr = std::to_string(n);
  if (asStr.length() % 2 != 0) {
    return false;
  }
  const auto half = asStr.length() / 2;
  for (auto i = 0; i < half; i++) {
    if (asStr[i] != asStr[i + half])
      return false;
  }
  return true;
}

bool isInvalid2(unsigned long n) {
  const auto asStr = std::to_string(n);
  const auto strLen = asStr.length();
  // Guess who learned about ranges
  return std::ranges::any_of(
      std::ranges::iota_view(1ul, (strLen / 2) + 1), [strLen, asStr](int len) {
        const auto res = std::div(strLen, len);
        if (res.rem != 0)
          return false;
        return std::ranges::all_of(
            std::ranges::iota_view(1, res.quot), [asStr, len](int groupIdx) {
              for (int i = 0; i < len; i++) {
                if (asStr[i] != asStr[len * groupIdx + i]) {
                  return false;
                }
              }
              return true;
            });
      });
}

std::pair<unsigned long, unsigned long> solve(const std::string_view line) {
  auto result1 = 0ul;
  auto result2 = 0ul;
  for (const auto range :
       line | std::ranges::views::split(std::string_view{","})) {
    auto parts = range | std::ranges::views::split(std::string_view{"-"});
    auto it = parts.begin();
    const auto a = parse<unsigned long>(std::ranges::to<std::string>(*it));
    const auto b = parse<unsigned long>(std::ranges::to<std::string>(*++it));
    for (unsigned long i = a; i <= b; i++) {
      result1 += isInvalid1(i) * i;
      result2 += isInvalid2(i) * i;
    }
  }
  return {result1, result2};
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

  const auto filePath = args.getOpt("-f").value_or("inputs/day02.in");
  const auto lines = readFileLines(filePath);

  if (!runPart1 && !runPart2) {
    std::cout << std::format("Unrecognized part: {}", part.value())
              << std::endl;
    return usage();
  }

  const auto [p1, p2] = solve(lines[0]);

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
