#pragma once
#include <functional>
#include <optional>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>
class CLIArguments {
private:
  std::unordered_map<std::string_view, std::string_view> options{};
  std::unordered_set<std::string_view> flags{};
  std::vector<std::string_view> positionals{};

public:
  std::string_view execName;

  CLIArguments(int argc, char *argv[]);

  std::optional<std::string_view> getOpt(std::string_view opt);

  bool hasFlag(std::string_view opt);

  std::optional<std::string_view> getPositional(int idx);

  bool runPart1();

  bool runPart2();
};

class CLIWrapper {
public:
  CLIWrapper(int argc, char *argv[], unsigned day);

  CLIArguments args;

  int run(std::function<void(CLIArguments &, std::vector<std::string>)> solve);

private:
  unsigned day;
};
