#include "error.h"
#include "lexer.h"
#include "parser.hh"
#include "type.h"
#include <fstream>
#include <iostream>
#include <string>

using namespace pyrite;

struct Options {
  std::string fileName;
  bool help = false;
  bool version = false;
  bool usage = false;

  Options(int argc, const char **argv) {
    if (argc == 1) {
      usage = true;
    } else {
      for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "-h" || arg == "--help") {
          help = true;
        } else if (arg == "-v" || arg == "--version") {
          version = true;
        } else if (arg[0] != '-') {
          fileName = arg;
        } else {
          std::cerr << "Unknown option: " << arg << std::endl;
          usage = true;
          break;
        }
      }
    }
  }
};

static void usage(std::string programFile) {
  std::cerr << "Usage: " << programFile << " [options] file" << std::endl;
  std::cerr << "Try '" << programFile << " --help' for more information."
            << std::endl;
}

static void help(std::string programFile) {
  std::cerr << "Usage: " << programFile << " [options] file" << std::endl;
  std::cerr << "Options:" << std::endl;
  std::cerr << "  -h, --help\t\tPrint this help message" << std::endl;
  std::cerr << "  -v, --version\t\tPrint version information" << std::endl;
}
#define MAJOR_VERSION "0"
#define MINOR_VERSION "1"
#define PATCH_VERSION "0"

#define VERSION_STRING MAJOR_VERSION "." MINOR_VERSION "." PATCH_VERSION

static void version() {
  std::cerr << "Pyrite version " << VERSION_STRING << std::endl;
}

int main(int argc, const char **argv) {
  Options options(argc, argv);
  if (options.usage) {
    usage(argv[0]);
  } else if (options.help) {
    help(argv[0]);
  } else if (options.version) {
    version();
  } else {
    std::ifstream file(options.fileName);
    if (!file) {
      std::cerr << "Could not open file: " << options.fileName << std::endl;
      return 1;
    }
    try {
      Lexer lexer(file);
      std::unique_ptr<AstNode> ast;
      Parser parser(lexer, argv[0], &ast);
      parser.parse();
      typeCheck(std::move(*ast));
      std::cout << astToString(*ast) << std::endl;
    } catch (const PyriteException &exception) {
      std::cerr << exception.getMessage() << std::endl;
      return 1;
    }
  }
  return 0;
}
