#include "codegen.h"
#include "error.h"
#include "lexer.h"
#include "parser.hh"
#include "type.h"
#include <fstream>
#include <iostream>
#include <string>

using namespace pyrite;
namespace pyrite {
std::vector<PyriteError> errors;
}

struct Options {
  std::string fileName;
  bool help = false;
  bool version = false;
  bool usage = false;
  std::string target;
  std::string outputFile;
  CodeFileType outputFileType = CodeFileType::OBJECT;
  OptimizationLevel optimizationLevel = OptimizationLevel::O2;

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
        } else if (arg == "-o" || arg == "--output") {
          if (++i < argc) {
            outputFile = argv[i];
          }
        } else if (arg == "-t" || arg == "--target") {
          if (++i < argc) {
            target = argv[i];
          }
        } else if (arg == "-c" || arg == "--compile") {
          outputFileType = CodeFileType::OBJECT;
        } else if (arg == "-S" || arg == "--assembly") {
          outputFileType = CodeFileType::ASSEMBLY;
        } else if (arg == "--emit-llvm") {
          if (outputFileType == CodeFileType::OBJECT) {
            outputFileType = CodeFileType::LLVM_BITCODE;
          } else if (outputFileType == CodeFileType::ASSEMBLY) {
            outputFileType = CodeFileType::LLVM_ASSEMBLY;
          }
        } else if (arg == "-O0") {
          optimizationLevel = OptimizationLevel::NONE;
        } else if (arg == "-O1") {
          optimizationLevel = OptimizationLevel::O1;
        } else if (arg == "-O2") {
          optimizationLevel = OptimizationLevel::O2;
        } else if (arg == "-O3") {
          optimizationLevel = OptimizationLevel::O3;
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
  std::cerr << "  -o, --output\t\tSpecify output file" << std::endl;
  std::cerr << "  -t, --target\t\tSpecify target triple" << std::endl;
  std::cerr << "  -c, --compile\t\tCompile to object file" << std::endl;
  std::cerr << "  -S, --assembly\tOutput to assembly code" << std::endl;
  std::cerr << "  --emit-llvm\t\tOutput LLVM IR" << std::endl;
  std::cerr << "  -O0\t\t\tDisable optimizations" << std::endl;
  std::cerr << "  -O1\t\t\tEnable simple optimizations" << std::endl;
  std::cerr << "  -O2\t\t\tEnable default optimizations" << std::endl;
  std::cerr << "  -O3\t\t\tEnable aggressive optimizations" << std::endl;
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
      ast = typeCheck(*ast);
      if (errors.size() > 0) {
        for (const auto &error : errors) {
          std::cerr << error.getMessage() << std::endl;
        }
        std::cerr << errors.size() << " errors" << std::endl;
        return 1;
      }
      ast = simplifyAst(*ast);
      // Provide default output file type if none was specified
      if (options.outputFile.empty()) {
        if (options.fileName.ends_with(".pyrite")) {
          options.outputFile =
              options.fileName.substr(0, options.fileName.size() - 7);
        } else {
          options.outputFile = options.fileName;
        }
        if (options.outputFileType == CodeFileType::OBJECT) {
          options.outputFile += ".o";
        } else if (options.outputFileType == CodeFileType::ASSEMBLY) {
          options.outputFile += ".s";
        } else if (options.outputFileType == CodeFileType::LLVM_BITCODE) {
          options.outputFile += ".bc";
        } else if (options.outputFileType == CodeFileType::LLVM_ASSEMBLY) {
          options.outputFile += ".ll";
        }
      }
      codegen(*ast, options.target, options.outputFile, options.outputFileType,
              options.optimizationLevel);
    } catch (const PyriteError &exception) {
      std::cerr << "Error thrown:" << std::endl;
      std::cerr << exception.getMessage() << std::endl;
      return 1;
    }catch (const std::exception &e) {
      std::cerr << "Error thrown:" << std::endl;
      std::cerr << e.what() << std::endl;
      return 1;
    }
  }
  return 0;
}
