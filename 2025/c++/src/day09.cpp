#include <algorithm>
#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <format>
#include <functional>
#include <iostream>
#include <print>
#include <ranges>
#include <string>
#include <string_view>
#include <utility>
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

namespace rv = std::ranges::views;
namespace r = std::ranges;
using point = std::pair<int64_t, int64_t>;
using segment = std::pair<point, point>;

std::int64_t area(const point &a, const point &b) {
  return (std::abs(a.first - b.first) + 1) *
         (1 + std::abs(a.second - b.second));
}

std::int64_t solve1(std::vector<point> input) {
  int64_t result = 0;
  for (int i = 0; i < input.size(); i++) {
    for (int j = i + 1; j < input.size(); j++) {
      result = std::max(result, area(input[i], input[j]));
    }
  }
  return result;
}

bool intersects(const segment &a, const segment &b) { return false; }

std::int64_t solve2(std::vector<point> inputs) {

  int64_t result = 0;

  return result;
}

int main(int argc, char *argv[]) {
  return CLIWrapper{argc, argv, 9}.run(
      [](CLIArguments &args, std::vector<std::string> lines) {
        const std::vector<point> input =
            lines | rv::transform([](auto l) {
              const auto parsed =
                  l | rv::split(',') | rv::transform([](auto token) {
                    return std::string_view(token);
                  }) |
                  rv::transform(parse<int64_t>) | r::to<std::vector>();
              return point{parsed[0], parsed[1]};
            }) |
            r::to<std::vector>();
        if (args.runPart1()) {
          auto p1 = solve1(input);
          std::println("Part1:\n{}", p1);
        }
        if (args.runPart2()) {
          auto p2 = solve2(input);
          std::println("Part2:\n{}", p2);
        }
      });
}
