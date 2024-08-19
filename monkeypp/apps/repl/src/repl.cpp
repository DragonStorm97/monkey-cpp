#include <exception>
#include <iomanip>
#include <iostream>
#include <limits>
#include "monkeypp.hpp"
#include <string>
#include <vector>
#include <ranges>
#include <format>


void tokenizer_repl()
{
  static constexpr auto type_width = 3;
  static constexpr auto type_string_width = 15;
  static constexpr auto num_initial_lines = 500;
  std::cout << ">>> ";
  std::vector<std::string> lines;
  lines.reserve(num_initial_lines);
  lines.emplace_back("");
  while (std::getline(std::cin, lines.back())) 
  {
    auto& line = lines.back();
    if (lines.back().empty()) { return; }

    const mpp::LexerState ils = mpp::Lexer::MakeState(line);

    for (auto ls = mpp::Lexer::NextToken(ils); ls.lastToken.type != mpp::TokenType::Eof; ls = mpp::Lexer::NextToken(ls))
    {
      std::cout << "Type: " << std::setfill('0') << std::setw(type_width) << static_cast<unsigned int>(ls.lastToken.type) 
        << " " << std::setfill(' ') << std::setw(type_string_width) << std::quoted(mpp::lookup::TokenString(ls.lastToken.type))
        << " | Literal: " << std::quoted(ls.lastToken.literal) << "\n";
   }

    std::cout << ">>> ";
    lines.emplace_back("");
  }
}

void parser_repl()
{
  std::cout << ">>> ";
  std::string line;
  while (std::getline(std::cin, line)) 
  {
    if (line.empty()) { return; }
    const mpp::LexerState ils = mpp::Lexer::MakeState(line);
    mpp::Parser parser{ ils };
    const auto program = parser.ParseProgram();
    if(!parser.errors.empty())
    {
      std::cout << "Parser Errors:\n\t";
      std::cout << (parser.errors 
        | std::views::transform([](const mpp::Parser::Error &err) -> std::string { return std::format("{} at: {}", err.error, err.position); })
        | std::views::join_with(std::string{"\n\t"})
        | std::ranges::to<std::string>()
      ) << "\n";
    }
    else
    {
      std::cout << "\n" << program.String() << "\n";
    }
    std::cout << ">>> ";
  }

}

void evaluator_repl()
{
  static constexpr auto num_initial_lines = 500;
  std::cout << ">>> ";
  mpp::Evaluator eval{};
  // TODO: this is gross that parse holds data that is used in evaluator
  //       we're overwriting the lexerState for each iteration here in parser, but string_views index into the lexerState... 
  //       (so to make it not shit the bed we store the strings outside of the loop)
  mpp::Parser parser;
  std::vector<std::string> lines;
  lines.reserve(num_initial_lines);
  lines.emplace_back("");
  while (std::getline(std::cin, lines.back())) 
  {
    auto& line = lines.back();
    if (lines.back().empty()) { return; }

    const mpp::LexerState ils = mpp::Lexer::MakeState(line);
    const auto program = parser.ParseProgram(ils);
    if(!parser.errors.empty())
    {
      std::cout << "Parser Errors:\n\t";
      std::cout << (parser.errors 
        | std::views::transform([](const mpp::Parser::Error &err) -> std::string { return std::format("{} at: {}", err.error, err.position); })
        | std::views::join_with(std::string{"\n\t"})
        | std::ranges::to<std::string>()
      ) << "\n";
    }
    else
    {
      auto res = eval(program);
      std::cout << res.String() << "\n";
    }
    std::cout << ">>> ";
    lines.emplace_back("");
  }

}

void repl() 
{

  std::cout << "repl started!\n";
  std::cout << "enter e/E for evaluator output\n";
  std::cout << "enter t/T for tokenizer output\n";
  std::cout << "enter p/P for Parser output\n";
  std::cout << "enter q/Q to quit\n";

  char opt = 'T';
  while (true)
  {
    std::cout << ">";
    std::cin >> opt;

    std::cout << "\n";
    std::cin.clear();
    std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
    switch (opt) {
    case 'e':
      [[fallthrough]];
    case 'E':
      evaluator_repl();
      return;
    case 't':
      [[fallthrough]];
    case 'T':
      tokenizer_repl();
      return;
    case 'p':
      [[fallthrough]];
    case 'P':
      parser_repl();
      return;
    case 'q':
      [[fallthrough]];
    case 'Q':
      std::cout << "\nQUIT!\n" << "\n";
      return;
      break;
    default:
      std::cout << "invalid option: " << opt << "\n";
      break;
    }
  }
}

// NOLINTNEXTLINE(bugprone-exception-escape)
int main() noexcept
try
{
  repl();
  std::cout << "\nrepl ended!" << "\n";
  std::cin.get();
}
catch (const std::exception& e)
{
  std::cout << e.what() << "\n";
}
