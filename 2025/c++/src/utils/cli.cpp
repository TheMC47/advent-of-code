#include "cli.h"
#include "utils/file.h"
#include <optional>
#include <print>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

CLIArguments::CLIArguments(int argc, char *argv[]) {
  execName = argv[0];
  for (int i = 1; i < argc; i++) {
    const std::string_view currStr{argv[i]};

    if (currStr[0] != '-') {
      positionals.push_back(currStr);
      continue;
    }

    const int nextIdx = i + 1;
    if (nextIdx == argc) {
      flags.emplace(currStr);
      continue;
    }
    const std::string_view nextStr{argv[nextIdx]};
    if (nextStr[0] == '-') {
      flags.emplace(currStr);
      continue;
    }
    options.emplace(currStr, nextStr);
    i++;
  }
}

std::optional<std::string_view> CLIArguments::getOpt(std::string_view opt) {
  const auto it = options.find(opt);
  if (it == options.end()) {
    return std::nullopt;
  }
  return {it->second};
}

std::optional<std::string_view> CLIArguments::getPositional(int idx) {
  if (idx >= positionals.size() || idx < 0) {
    return std::nullopt;
  }
  return {positionals[idx]};
}

bool CLIArguments::hasFlag(std::string_view flag) {
  return flags.find(flag) != flags.end();
}

bool CLIArguments::runPart1() {
  const auto part = getPositional(0);
  return !part.has_value() || part.value() == "1";
}

bool CLIArguments::runPart2() {
  const auto part = getPositional(0);
  return !part.has_value() || part.value() == "2";
}

CLIWrapper::CLIWrapper(int argc, char *argv[], unsigned day)
    : args(argc, argv), day(day) {}

int CLIWrapper::run(
    std::function<void(CLIArguments &, std::vector<std::string>)> solve) {
  auto usage = [this]() {
    std::println("Usage: {} <part> (part: 1, 2)", this->args.execName);
    return 1;
  };
  const auto part = args.getPositional(0);

  if (!args.runPart1() && !args.runPart2()) {
    std::println("Unrecognized part: {}", part.value());
    return usage();
  }
  const auto filePath =
      args.getOpt("-f").value_or(std::format("inputs/day{:02}.in", day));
  const auto lines = readFileLines(filePath);

  solve(args, lines);
  return 0;
}
