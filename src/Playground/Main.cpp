#include <map>
#include <string>

#include <CLI/CLI.hpp>

static CLI::App& app() {
    static CLI::App a;
    return a;
}
static std::map<CLI::App*, int (*)(CLI::App*)>& subcommands() {
    static std::map<CLI::App*, int (*)(CLI::App*)> s;
    return s;
}

CLI::App* addSubcommand(std::string name, int (*mainFn)(CLI::App*)) {
    auto* cmd = app().add_subcommand(std::move(name));
    subcommands().insert({ cmd, mainFn });
    return cmd;
}

int main(int argc, char* argv[]) {
    CLI11_PARSE(app(), argc, argv)
    for (auto [subcmd, submain]: subcommands()) {
        if (subcmd->parsed()) submain(subcmd);
    }
}
