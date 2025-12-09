#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <format>
#include <iostream>
#include <print>
#include <ranges>
#include <set>
#include <string>
#include <string_view>
#include <tuple>
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

using point = std::pair<unsigned, std::tuple<double, double, double>>;

double distance(const point &a, const point &b) {
  return pow(std::get<0>(a.second) - std::get<0>(b.second), 2) +
         pow(std::get<1>(a.second) - std::get<1>(b.second), 2) +
         pow(std::get<2>(a.second) - std::get<2>(b.second), 2);
}

using edge = std::pair<point, point>;

double length(const edge &e) { return distance(e.first, e.second); }

bool isLonger(const edge &a, const edge &b) { return length(a) > length(b); }

std::pair<double, double> solve(std::vector<std::string> inputs,
                                unsigned iterations) {
  std::vector<point> points =
      inputs | std::views::enumerate |
      std::ranges::views::transform([](auto ir) {
        const auto [idx, r] = ir;
        const auto v =
            r | std::ranges::views::split(',') |
            std::ranges::views::transform([](auto t) {
              return parse<uint64_t>(std::string(std::begin(t), std::end(t)));
            }) |
            std::ranges::to<std::vector>();
        return point{idx, std::make_tuple(v[0], v[1], v[2])};
      }) |
      std::ranges::to<std::vector>();

  std::vector<edge> edges;
  edges.reserve(points.size() * points.size());

  for (unsigned i = 0; i < points.size(); i++) {
    for (unsigned j = i + 1; j < points.size(); j++) {
      edges.push_back({points[i], points[j]});
    }
  }

  std::make_heap(edges.begin(), edges.end(), isLonger);

  std::vector<std::pair<unsigned, unsigned>> sets =
      points | std::ranges::views::transform([](auto &r) {
        return std::make_pair(r.first, 1u);
      }) |
      std::ranges::to<std::vector>();

  const auto find = [&sets](unsigned x) {
    auto root = x;
    while (sets[root].first != root) {
      root = sets[root].first;
    }

    while (sets[x].first != root) {
      const auto px = sets[x].first;
      sets[x].first = px;
      x = px;
    }

    return root;
  };

  const auto _union = [&sets, &find](unsigned x, unsigned y) {
    x = find(x);
    y = find(y);

    if (x == y) {
      return;
    }

    if (sets[x].second < sets[y].second) {
      std::swap(x, y);
    }
    sets[y].first = x;
    sets[x].second += sets[y].second;
  };

  for (int i = 0; i < iterations; i++) {
    std::pop_heap(edges.begin(), edges.end(), isLonger);
    auto top = edges.back();
    _union(top.first.first, top.second.first);
    edges.pop_back();
  }
  auto copySets = sets;
  std::ranges::sort(copySets, std::greater(), [](auto p) { return p.second; });

  auto p1 = copySets[0].second * copySets[1].second * copySets[2].second;

  auto p2 = 0ul;

  while (sets[find(0)].second != sets.size()) {
    std::pop_heap(edges.begin(), edges.end(), isLonger);
    auto top = edges.back();
    _union(top.first.first, top.second.first);
    p2 = std::get<0>(top.first.second) * std::get<0>(top.second.second);
    edges.pop_back();
  }

  return {p1, p2};
}

int main(int argc, char *argv[]) {
  return CLIWrapper{argc, argv, 8}.run(
      [](CLIArguments &args, std::vector<std::string> lines) {
        const auto iterations =
            args.getOpt("-i").transform(parse<unsigned>).value_or(1000);
        auto [p1, p2] = solve(lines, iterations);
        if (args.runPart1()) {
          std::println("Part1:\n{}", p1);
        }
        if (args.runPart2()) {
          std::println("Part2:\n{}", p2);
        }
      });
}
