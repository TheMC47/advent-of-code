#include "cli.h"
#include <optional>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

CLIArguments::CLIArguments(int argc, char *argv[]) {
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
