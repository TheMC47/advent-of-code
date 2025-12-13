#include <cassert>
#include <cstdint>
#include <format>
#include <functional>
#include <iterator>
#include <numeric>
#include <print>
#include <ranges>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <utils/cli.h>
#include <utils/file.h>
#include <vector>

namespace rv = std::ranges::views;
namespace r = std::ranges;

std::uint64_t solve1(std::vector<std::string> &input) {
  auto lines =
      input | rv::drop(30) | rv::transform([](auto r) {
        unsigned width = std::stoi(std::string(r.begin(), r.begin() + 2));
        unsigned height = std::stoi(std::string(r.begin() + 3, r.begin() + 5));
        std::vector<unsigned> rest =
            r | rv::drop(7) | rv::split(' ') |
            rv::transform([](auto sub) -> unsigned {
              return std::stoi(std::string(sub.begin(), sub.end()));
            }) |
            r::to<std::vector>();

        return std::make_pair(std::make_pair(width, height), rest);
      });

  unsigned result = 0;

  for (const auto &l : lines) {
    const auto [wh, rest] = l;
    const auto [w, h] = wh;
    const auto total = std::accumulate(std::begin(rest), std::end(rest), 0);
    if (9 * total <= w * h)
      result++;
  }

  return result;
}

std::uint64_t solve2(std::vector<std::string> &input) { return 0; }

std::pair<std::string, std::vector<std::string>> parseLine(std::string line) {
  auto nodes = line | rv::split(' ') | rv::transform([](auto r) {
                 return std::string(std::begin(r), std::end(r));
               }) |
               r::to<std::vector>();
  return {std::string(std::begin(nodes[0]), std::end(nodes[0]) - 1),
          nodes | rv::drop(1) | r::to<std::vector>()};
}

int main(int argc, char *argv[]) {
  return CLIWrapper{argc, argv, 12}.run(
      [](CLIArguments &args, std::vector<std::string> lines) {
        if (args.runPart1()) {
          auto p1 = solve1(lines);
          std::println("Part1:\n{}", p1);
        }
        if (args.runPart2()) {
          auto p2 = solve2(lines);
          std::println("Part2:\n{}", p2);
        }
      });
}
