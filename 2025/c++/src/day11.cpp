#include <algorithm>
#include <cassert>
#include <chrono>
#include <cstdint>
#include <format>
#include <fstream>
#include <functional>
#include <iterator>
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

using Graph = std::unordered_map<std::string, std::vector<std::string>>;

std::uint64_t dfs(const Graph &g, std::unordered_set<std::string> &seen,
                  const std::string &from) {
  if (from == "out") {
    return 1;
  }
  seen.emplace(from);

  std::uint64_t result = 0;

  for (const auto &e : g.at(from)) {
    if (!seen.contains(e)) {
      result += dfs(g, seen, e);
    }
  }

  seen.erase(from);

  return result;
}

std::uint64_t solve1(Graph &g) {
  std::unordered_set<std::string> seen;
  return dfs(g, seen, "you");
}

std::uint64_t dfs2(const Graph &g, std::unordered_set<std::string> &seen,
                   const std::string &from, const std::string &to,
                   const std::vector<std::string> &terminals) {
  if (from == to) {
    return 1;
  }
  if (r::find(terminals, from) != std::end(terminals)) {
    return 0;
  }

  if (!g.contains(from)) {
    return 0;
  }

  seen.emplace(from);

  std::uint64_t result = 0;

  for (const auto &e : g.at(from)) {
    if (!seen.contains(e)) {
      result += dfs2(g, seen, e, to, terminals);
    }
  }

  seen.erase(from);

  return result;
}

std::uint64_t solve2(Graph &g) {
  const std::vector<std::string> boundary1{"ngu", "wti", "wix", "yfo"};
  const std::vector<std::string> boundary2{"xgo", "gih", "vgm"};
  const std::vector<std::string> boundary3{"ebj", "jak", "zil", "mlk"};
  const std::vector<std::string> boundary4{"gza", "eee", "zgg"};
  const std::vector<std::string> boundary5{"qfg", "rid", "you"};

  const auto doDFS = [&g](const std::string &from, const std::string &tos,
                          const std::vector<std::string> &boundary) {
    std::unordered_set<std::string> seen;
    return dfs2(g, seen, from, tos, boundary);
  };

  std::uint64_t result = 0;
  for (const auto &[b1, b2, b3, b4, b5] : rv::cartesian_product(
           boundary1, boundary2, boundary3, boundary4, boundary5)) {
    result += doDFS("svr", b1, boundary1) * doDFS(b1, "fft", boundary2) *
              doDFS("fft", b2, boundary2) * doDFS(b2, b3, boundary3) *
              doDFS(b3, b4, boundary4) * doDFS(b4, "dac", boundary5) *
              doDFS("dac", b5, boundary5) * doDFS(b5, "out", {});
  }

  return result;
}

struct pair_hash {
  template <class T1, class T2>
  std::size_t operator()(const std::pair<T1, T2> &p) const {
    auto h1 = std::hash<T1>{}(p.first);
    auto h2 = std::hash<T2>{}(p.second);
    return h1 ^ h2;
  }
};

std::uint64_t dfs2Fast(const Graph &g, std::unordered_set<std::string> &seen,
                       const std::string &from, const std::string &to,
                       std::unordered_map<std::pair<std::string, std::string>,
                                          uint64_t, pair_hash> &memo) {
  if (from == to) {
    return 1;
  }
  if (memo.contains({from, to})) {
    return memo[{from, to}];
  }

  if (!g.contains(from)) {
    return 0;
  }

  seen.emplace(from);

  std::uint64_t result = 0;

  for (const auto &e : g.at(from)) {
    if (!seen.contains(e)) {
      result += dfs2Fast(g, seen, e, to, memo);
    }
  }

  seen.erase(from);
  memo[{from, to}] = result;

  return result;
}

std::uint64_t solve2Fast(Graph &g) {
  std::unordered_set<std::string> seen;
  std::unordered_map<std::pair<std::string, std::string>, uint64_t, pair_hash>
      memo;
  return dfs2Fast(g, seen, "svr", "fft", memo) *
         dfs2Fast(g, seen, "fft", "dac", memo) *
         dfs2Fast(g, seen, "dac", "out", memo);
}

std::pair<std::string, std::vector<std::string>> parseLine(std::string line) {
  auto nodes = line | rv::split(' ') | rv::transform([](auto r) {
                 return std::string(std::begin(r), std::end(r));
               }) |
               r::to<std::vector>();
  return {std::string(std::begin(nodes[0]), std::end(nodes[0]) - 1),
          nodes | rv::drop(1) | r::to<std::vector>()};
}

int main(int argc, char *argv[]) {
  return CLIWrapper{argc, argv, 11}.run(
      [](CLIArguments &args, std::vector<std::string> lines) {
        Graph input =
            lines | rv::transform(parseLine) | r::to<std::unordered_map>();

        if (args.hasFlag("-d")) {
          std::ofstream out("graph.dot");
          out << "digraph G {\n";
          for (auto &kv : input) {
            for (auto &e : kv.second) {
              out << "  \"" << kv.first << "\" -> \"" << e << "\";\n";
            }
          }
          out << "  \"svr\" [style=filled, fillcolor=yellow];\n";
          out << "  \"dac\" [style=filled, fillcolor=lightblue];\n";
          out << "  \"fft\" [style=filled, fillcolor=lightgreen];\n";
          out << "  \"out\" [style=filled, fillcolor=red];\n";
          out << "}\n";
          return;
        }

        if (args.runPart1()) {
          auto p1 = solve1(input);
          std::println("Part1:\n{}", p1);
        }
        if (args.runPart2()) {
          const auto startTime = std::chrono::high_resolution_clock::now();
          auto p2f = solve2Fast(input);
          const auto endTime = std::chrono::high_resolution_clock::now();
          std::println("Part2 (fast):\n{}\ntime:{}us", p2f,
                       std::chrono::duration_cast<std::chrono::microseconds>(
                           endTime - startTime)
                           .count());

          const auto startTime2 = std::chrono::high_resolution_clock::now();
          auto p2 = solve2(input);
          const auto endTime2 = std::chrono::high_resolution_clock::now();
          std::println("Part2:\n{}\ntime:{}us", p2,
                       std::chrono::duration_cast<std::chrono::microseconds>(
                           endTime2 - startTime2)
                           .count());
        }
      });
}
