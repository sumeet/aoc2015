#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <variant>

// badboy from https://www.bfilipek.com/2018/06/variant.html#overload
template<class... Ts>
struct overload : Ts... { using Ts::operator()...; };
template<class... Ts>
overload(Ts...) -> overload<Ts...>;

const char *SAMPLE = R"""(123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i)""";

struct And;
struct Or;
struct Not;
struct LShift;
struct RShift;

typedef std::variant<unsigned short, And, Or, LShift, RShift, Not> instruction;

typedef struct And {
    std::string lhs;
    std::string rhs;
} And;

typedef struct Or {
    std::string lhs;
    std::string rhs;
} Or;

typedef struct LShift {
    std::string name;
    unsigned short amount;
} LShift;

typedef struct RShift {
    std::string name;
    unsigned short amount;
} RShift;

typedef struct Not {
    std::string name;
} Not;

bool contains(const std::string &needle, const std::string &haystack) {
    return haystack.find(needle) != std::string::npos;
}

std::vector<std::string> split(const std::string &delim, const std::string &str) {
    std::vector<std::string> cont;
    int current, previous = 0;
    current = str.find(delim);
    while (current != std::string::npos) {
        cont.push_back(str.substr(previous, current - previous));
        previous = current + delim.length();
        current = str.find(delim, previous);
    }
    cont.push_back(str.substr(previous, current - previous));
    return cont;
}

unsigned short interpret(const std::unordered_map<std::string, instruction> &map, const std::string &name) {
    return std::visit(overload{
                              [](unsigned short i) { return i; },
                              [map](And a) { return static_cast<unsigned short>(interpret(map, a.lhs) & interpret(map, a.rhs)); },
                              [map](Or a) { return static_cast<unsigned short>(interpret(map, a.lhs) & interpret(map, a.rhs)); },
                              [map](LShift lshift) { return static_cast<unsigned short>(interpret(map, lshift.name) << lshift.amount); },
                              [map](RShift rshift) { return static_cast<unsigned short>(interpret(map, rshift.name) << rshift.amount); },
                              [map](Not n) { return static_cast<unsigned short>(~interpret(map, n.name)); },
                      },
                      map.at(name));
}

instruction parse_instruction(const std::string &s) {
    if (contains("NOT", s)) {
        auto name = split("NOT ", s)[1];
        return Not{name};
    } else if (contains("AND", s)) {
        auto lr = split(" AND ", s);
        return And{lr[0], lr[1]};
    } else if (contains("OR", s)) {
        auto lr = split(" OR ", s);
        return Or{lr[0], lr[1]};
    } else if (contains("LSHIFT", s)) {
        auto lr = split(" LSHIFT ", s);
        return LShift{lr[0], static_cast<unsigned short>(std::stoi(lr[1]))};
    } else if (contains("RSHIFT", s)) {
        auto lr = split(" RSHIFT ", s);
        return RShift{lr[0], static_cast<unsigned short>(std::stoi(lr[1]))};
    } else {
        return static_cast<unsigned short>(std::stoi(s));
    }
}

int main() {
    std::unordered_map<std::string, instruction> map;

    std::stringstream ss(SAMPLE);
    std::string line;
    while (std::getline(ss, line, '\n')) {
        auto sides = split(" -> ", line);
        auto name = sides[1];
        auto instruction = sides[0];
        map.insert({name, parse_instruction(instruction)});
        auto test = split("HELLO AND BYE", " AND ");
    }

    printf("%u\n", interpret(map, "h"));
    return 0;
}
