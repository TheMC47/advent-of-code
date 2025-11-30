#include <charconv>
#include <format>
#include <iostream>
#include <optional>
#include <ostream>
#include <string_view>
#include <system_error>

std::string part1() { return "Hello, World!"; }

std::string part2() { return "Hello, World 2!"; }

[[nodiscard]] std::optional<int> safeParseInt(std::string_view str) noexcept {
    auto begin = str.data();
    auto end = str.data() + str.size();
    int out{};
    auto res = std::from_chars(begin, end, out);
    if (res.ec == std::errc{} && res.ptr == end) {
        return out;
    }
    return std::nullopt;
}

int main(int argc, char *argv[]) {
    auto usage = [argv]() {
        std::cout << std::format("Usage: {} [part] (part: 1, 2)", argv[0])
                  << std::endl;
        return 1;
    };

    if (argc < 2) {
        return usage();
    }

    auto dayStr = argv[1];
    const auto day = safeParseInt(dayStr);

    if (!day.has_value()) {
        std::cout << std::format("Argument is not a number: {}", dayStr)
                  << std::endl;
        return usage();
    }
    switch (*day) {
    case 1:
        std::cout << part1() << std::endl;
        break;
    case 2:
        std::cout << part2() << std::endl;
        break;
    default:
        std::cout << std::format("Unrecognized part: {}", dayStr) << std::endl;
        return usage();
    }
    return 0;
}
