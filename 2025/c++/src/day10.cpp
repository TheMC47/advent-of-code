#include <algorithm>
#include <bitset>
#include <cassert>
#include <cstdint>
#include <format>
#include <functional>
#include <iostream>
#include <print>
#include <queue>
#include <ranges>
#include <string>
#include <string_view>
#include <utils/cli.h>
#include <utils/file.h>
#include <vector>

namespace rv = std::ranges::views;
namespace r = std::ranges;

using State = std::bitset<16>;

struct Machine {
  State state;
  std::vector<std::vector<std::uint64_t>> buttons;
  std::vector<std::uint64_t> joltage;
};

template <typename N> N parse(std::string_view sv) {
  N out;
  const auto result = std::from_chars(sv.data(), sv.data() + sv.size(), out);
  if (result.ec != std::errc()) {
    std::cerr << std::make_error_code(result.ec).message() << std::endl;
    throw std::runtime_error(std::format("Could not read number from {}", sv));
  }
  return out;
}

Machine parseLine(std::string_view line) {
  assert(line[0] == '[');
  Machine m{};

  unsigned idx = 1;

  while (line[idx] != ']') {
    m.state[idx - 1] = line[idx] == '#';
    idx++;
  }
  idx += 2;
  while (line[idx] != '{') {
    assert(line[idx] == '(');
    const auto end = line.find(")", idx);
    const auto buttonStr = line.substr(idx + 1, end - idx - 1);
    m.buttons.push_back(buttonStr | rv::split(',') | rv::transform([](auto t) {
                          return std::string_view(std::begin(t), std::end(t));
                        }) |
                        rv::transform(parse<std::uint64_t>) |
                        r::to<std::vector>());
    idx = end + 2;
  }

  m.joltage = line.substr(idx + 1, line.size() - idx - 2) | rv::split(',') |
              rv::transform([](auto t) {
                return std::string_view(std::begin(t), std::end(t));
              }) |
              rv::transform(parse<std::uint64_t>) | r::to<std::vector>();

  return m;
}

State toggle(const State &current, const std::vector<std::uint64_t> &button) {
  State ret = current;
  for (const auto &b : button) {
    ret.flip(b);
  }
  return ret;
}

std::string format(const State &s) {
  std::string str = "";
  for (int i = 0; i < 16; i++) {
    str.append(s.test(i) ? "#" : ".");
  }
  return str;
}

std::uint64_t solve1(std::vector<Machine> &machines) {
  std::uint64_t result = 0;
  for (auto &m : machines) {
    std::unordered_set<State> seen;
    std::queue<std::pair<State, std::uint64_t>> queue;

    queue.push({{}, 0});
    seen.insert({});

    while (!queue.empty()) {
      auto const [currentState, numSwitches] = queue.front();
      if (currentState == m.state) {
        result += numSwitches;
        break;
      }
      queue.pop();
      for (const auto &button : m.buttons) {
        const auto nextState = toggle(currentState, button);
        if (!seen.contains(nextState)) {
          seen.insert(nextState);
          queue.push({nextState, numSwitches + 1});
        }
      }
    }
  }
  return result;
}

template <> struct std::hash<std::vector<uint64_t>> {
  std::size_t operator()(std::vector<uint64_t> const &vec) const {
    std::size_t seed = vec.size();
    for (auto &i : vec) {
      seed ^= i + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    }
    return seed;
  }
};

std::vector<uint64_t> jolt(const std::vector<uint64_t> &current,
                           const std::vector<std::uint64_t> &button) {
  std::vector<uint64_t> ret = current;
  for (const auto &b : button) {
    ret[b]++;
  }
  return ret;
}

std::uint64_t solve2(std::vector<Machine> &machines) {
  std::uint64_t result = 0;
  for (auto &m : machines) {
    std::unordered_set<std::vector<std::uint64_t>> seen;
    std::queue<std::pair<std::vector<std::uint64_t>, std::uint64_t>> queue;

    std::vector<std::uint64_t> v(m.joltage.size(), 0);
    queue.push({v, 0});
    seen.insert(v);

    while (!queue.empty()) {
      auto const [currentState, numSwitches] = queue.front();
      if (currentState == m.joltage) {
        result += numSwitches;
        break;
      }
      queue.pop();
      for (const auto &button : m.buttons) {
        const auto nextState = jolt(currentState, button);

        const auto valid = r::all_of(nextState | rv::enumerate, [&m](auto x) {
          const auto [i, joltageI] = x;
          return joltageI <= m.joltage[i];
        });

        if (valid && !seen.contains(nextState)) {
          seen.insert(nextState);
          queue.push({nextState, numSwitches + 1});
        }
      }
    }
  }
  return result;
}

int main(int argc, char *argv[]) {
  return CLIWrapper{argc, argv, 10}.run(
      [](CLIArguments &args, std::vector<std::string> lines) {
        std::vector<Machine> input =
            lines | rv::transform(parseLine) | r::to<std::vector>();
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
