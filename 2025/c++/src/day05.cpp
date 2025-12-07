#include <algorithm>
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

using interval = std::pair<unsigned long long, unsigned long long>;

template <typename N> N parse(std::string_view sv) {
  N out;
  const auto result = std::from_chars(sv.data(), sv.data() + sv.size(), out);
  if (result.ec != std::errc()) {
    std::cerr << std::make_error_code(result.ec).message() << std::endl;
    throw std::runtime_error(std::format("Could not read number from {}", sv));
  }
  return out;
}

class Input {
public:
  std::vector<unsigned long long> probes;
  std::vector<std::pair<unsigned long long, unsigned long long>> intervals;
  Input(const std::vector<std::string> &lines) {
    auto idx = 0ul;
    for (; lines[idx] != ""; idx++) {
      auto split = lines[idx] | std::ranges::views::split(('-')) |
                   std::views::transform([](auto &&r) {
                     return parse<unsigned long long>(std::string_view(r));
                   }) |
                   std::ranges::to<std::vector<unsigned long long>>();
      intervals.push_back({split[0], split[1]});
    }
    std::ranges::sort(intervals);
    idx++;

    for (; idx < lines.size(); idx++) {
      probes.push_back(parse<unsigned long long>(lines[idx]));
    }
  }
};

bool isInInterval(const unsigned long long x, const interval &p) {
  return x >= p.first && x <= p.second;
}

void wait() {
  std::string name;
  std::getline(std::cin, name);
}

std::optional<interval> isContained(unsigned long long x,
                                    const std::vector<interval> &intervals,
                                    size_t from, size_t to) {
  if (from >= to) {
    return std::nullopt;
  }
  const auto mid = from + (to - from) / 2;

  const auto &p = intervals[mid];

  if (isInInterval(x, p)) {
    return p;
  }

  if (x < p.first) {
    return isContained(x, intervals, from, mid);
  }

  return isContained(x, intervals, mid + 1, to);
}

unsigned long long solve1(const Input &input) {
  // TODO this didn't work
  // return std::ranges::count_if(
  //     input.probes, [&input](unsigned long long probe) {
  //       std::println("---------\nChecking {}", probe);
  //       return isContained(probe, input.intervals, 0, input.intervals.size())
  //           .has_value();
  //     });
  // DEBUG:
  // unsigned result = 0ul;
  // for (const auto &probe : input.probes) {
  //   std::println("Testing {}", probe);
  //   const auto binary =
  //       isContained(probe, input.intervals, 0, input.intervals.size());

  //   if (binary.has_value()) {
  //     std::println("[binary] Found {} for {}", *binary, probe);
  //   }
  //   bool foundBrute = false;
  //   for (const auto &interval : input.intervals) {
  //     if (isInInterval(probe, interval)) {
  //       foundBrute = true;
  //       result++;
  //       std::println("[brute] Found {} for {}", interval, probe);
  //       break;
  //     }
  //   }
  //   if (foundBrute != binary.has_value()) {
  //     return -1;
  //   }
  // }
  // return result;

  return std::ranges::count_if(input.probes, [&input](
                                                 unsigned long long probe) {
    return std::ranges::any_of(input.intervals, [&probe](const auto &interval) {
      return isInInterval(probe, interval);
    });
  });
}

unsigned long solve2(const Input &input) {
  if (input.intervals.size() == 0) {
    return 0;
  }

  auto current = input.intervals[0];
  auto result = 0ul;

  const auto updateResult = [&result, &current] {
    result += current.second - current.first + 1;
  };

  for (const auto &next : input.intervals | std::ranges::views::drop(1)) {
    if (isInInterval(next.first, current)) {
      current.second = std::max(next.second, current.second);
      continue;
    }
    updateResult();
    current = next;
  }
  updateResult();
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

  const auto filePath = args.getOpt("-f").value_or("inputs/day05.in");
  const auto lines = readFileLines(filePath);

  if (!runPart1 && !runPart2) {
    std::cout << std::format("Unrecognized part: {}", part.value())
              << std::endl;
    return usage();
  }

  Input input{lines};

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
