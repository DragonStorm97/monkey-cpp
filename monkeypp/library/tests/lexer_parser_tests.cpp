

#include "monkeypp.hpp"
#include <array>
#include <cstddef>
#include <ranges>
#include <memory>
#include <string>
#include <string_view>

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_message.hpp>

template<std::size_t N> 
[[nodiscard]] constexpr auto TestLexerTokens(mpp::LexerState lexerState, std::array<mpp::Token, N> comps)
{
  std::array<bool, N> res{ false };

  for (auto [result, expected] : std::views::zip(res, comps))
  {
    lexerState = mpp::Lexer::NextToken(lexerState);
    result = lexerState.lastToken == expected;
  }
  lexerState = mpp::Lexer::NextToken(lexerState);

  return std::make_pair(lexerState.lastToken.type == mpp::TokenType::Eof, res);
}

TEST_CASE("Lexer correctly tokenizes", "[lexer]")
{
  static constexpr std::string_view input = R"(
    let five = 5;
    let ten = 10;
    let add = fn(x, y) {
      x + y;
    };
    let result = add(five, ten);

    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
      return true;
    } else {
      return false;
    }
    
    10 == 10;
    10 != 9;

    let str = "Hello World!";

    [1,2];
    {1:2, 3:4};
  )";

  static constexpr auto init_state = mpp::Lexer::MakeState(input);
  static constexpr auto mps = TestLexerTokens(init_state, 
    std::array {
      mpp::Token{ mpp::TokenType::Let },
      mpp::Token{ mpp::TokenType::Ident, "five" },
      mpp::Token{ mpp::TokenType::Assign },
      mpp::Token{ mpp::TokenType::Int, "5" },
      mpp::Token{ mpp::TokenType::Semicolon },
      mpp::Token{ mpp::TokenType::Let },
      mpp::Token{ mpp::TokenType::Ident, "ten" },
      mpp::Token{ mpp::TokenType::Assign },
      mpp::Token{ mpp::TokenType::Int, "10" },
      mpp::Token{ mpp::TokenType::Semicolon },
      mpp::Token{ mpp::TokenType::Let },
      mpp::Token{ mpp::TokenType::Ident, "add" },
      mpp::Token{ mpp::TokenType::Assign },
      mpp::Token{ mpp::TokenType::Function },
      mpp::Token{ mpp::TokenType::LParen },
      mpp::Token{ mpp::TokenType::Ident, "x" },
      mpp::Token{ mpp::TokenType::Comma },
      mpp::Token{ mpp::TokenType::Ident, "y" },
      mpp::Token{ mpp::TokenType::RParen },
      mpp::Token{ mpp::TokenType::LBrace },
      mpp::Token{ mpp::TokenType::Ident, "x" },
      mpp::Token{ mpp::TokenType::Plus },
      mpp::Token{ mpp::TokenType::Ident, "y" },
      mpp::Token{ mpp::TokenType::Semicolon },
      mpp::Token{ mpp::TokenType::RBrace },
      mpp::Token{ mpp::TokenType::Semicolon },
      mpp::Token{ mpp::TokenType::Let },
      mpp::Token{ mpp::TokenType::Ident, "result" },
      mpp::Token{ mpp::TokenType::Assign },
      mpp::Token{ mpp::TokenType::Ident, "add" },
      mpp::Token{ mpp::TokenType::LParen },
      mpp::Token{ mpp::TokenType::Ident, "five" },
      mpp::Token{ mpp::TokenType::Comma },
      mpp::Token{ mpp::TokenType::Ident, "ten" },
      mpp::Token{ mpp::TokenType::RParen },
      mpp::Token{ mpp::TokenType::Semicolon },
      mpp::Token{ mpp::TokenType::Bang },
      mpp::Token{ mpp::TokenType::Minus },
      mpp::Token{ mpp::TokenType::Slash },
      mpp::Token{ mpp::TokenType::Asterisk },
      mpp::Token{ mpp::TokenType::Int, "5" },
      mpp::Token{ mpp::TokenType::Semicolon },
      mpp::Token{ mpp::TokenType::Int, "5" },
      mpp::Token{ mpp::TokenType::LessThan },
      mpp::Token{ mpp::TokenType::Int, "10" },
      mpp::Token{ mpp::TokenType::GreaterThan },
      mpp::Token{ mpp::TokenType::Int, "5" },
      mpp::Token{ mpp::TokenType::Semicolon },
      mpp::Token{ mpp::TokenType::If },
      mpp::Token{ mpp::TokenType::LParen },
      mpp::Token{ mpp::TokenType::Int, "5" },
      mpp::Token{ mpp::TokenType::LessThan },
      mpp::Token{ mpp::TokenType::Int, "10" },
      mpp::Token{ mpp::TokenType::RParen },
      mpp::Token{ mpp::TokenType::LBrace },
      mpp::Token{ mpp::TokenType::Return },
      mpp::Token{ mpp::TokenType::True },
      mpp::Token{ mpp::TokenType::Semicolon },
      mpp::Token{ mpp::TokenType::RBrace },
      mpp::Token{ mpp::TokenType::Else },
      mpp::Token{ mpp::TokenType::LBrace },
      mpp::Token{ mpp::TokenType::Return },
      mpp::Token{ mpp::TokenType::False },
      mpp::Token{ mpp::TokenType::Semicolon },
      mpp::Token{ mpp::TokenType::RBrace },
      mpp::Token{ mpp::TokenType::Int, "10" },
      mpp::Token{ mpp::TokenType::CompEqual },
      mpp::Token{ mpp::TokenType::Int, "10" },
      mpp::Token{ mpp::TokenType::Semicolon },
      mpp::Token{ mpp::TokenType::Int, "10" },
      mpp::Token{ mpp::TokenType::CompNotEqual },
      mpp::Token{ mpp::TokenType::Int, "9" },
      mpp::Token{ mpp::TokenType::Semicolon},
      mpp::Token{ mpp::TokenType::Let},
      mpp::Token{ mpp::TokenType::Ident, "str" },
      mpp::Token{ mpp::TokenType::Assign},
      mpp::Token{ mpp::TokenType::String, "Hello World!" },
      mpp::Token{ mpp::TokenType::Semicolon},
      mpp::Token{mpp::TokenType::LBracket},
      mpp::Token{ mpp::TokenType::Int, "1" },
      mpp::Token{mpp::TokenType::Comma},
      mpp::Token{ mpp::TokenType::Int, "2" },
      mpp::Token{mpp::TokenType::RBracket},
      mpp::Token{ mpp::TokenType::Semicolon},
      mpp::Token{mpp::TokenType::LBrace},
      mpp::Token{ mpp::TokenType::Int, "1" },
      mpp::Token{mpp::TokenType::Colon},
      mpp::Token{ mpp::TokenType::Int, "2" },
      mpp::Token{mpp::TokenType::Comma},
      mpp::Token{ mpp::TokenType::Int, "3" },
      mpp::Token{mpp::TokenType::Colon},
      mpp::Token{ mpp::TokenType::Int, "4" },
      mpp::Token{mpp::TokenType::RBrace},
      mpp::Token{ mpp::TokenType::Semicolon}
    });

  STATIC_REQUIRE(mps.first);
  STATIC_REQUIRE(mps.second[0]);
  STATIC_REQUIRE(mps.second[1]);
  STATIC_REQUIRE(mps.second[2]);
  STATIC_REQUIRE(mps.second[3]);
  STATIC_REQUIRE(mps.second[4]);
  STATIC_REQUIRE(mps.second[5]);
  STATIC_REQUIRE(mps.second[6]);
  STATIC_REQUIRE(mps.second[7]);
  STATIC_REQUIRE(mps.second[8]);
  STATIC_REQUIRE(mps.second[9]);
  STATIC_REQUIRE(mps.second[10]);
  STATIC_REQUIRE(mps.second[11]);
  STATIC_REQUIRE(mps.second[12]);
  STATIC_REQUIRE(mps.second[13]);
  STATIC_REQUIRE(mps.second[14]);
  STATIC_REQUIRE(mps.second[15]);
  STATIC_REQUIRE(mps.second[16]);
  STATIC_REQUIRE(mps.second[17]);
  STATIC_REQUIRE(mps.second[18]);
  STATIC_REQUIRE(mps.second[19]);
  STATIC_REQUIRE(mps.second[20]);
  STATIC_REQUIRE(mps.second[21]);
  STATIC_REQUIRE(mps.second[22]);
  STATIC_REQUIRE(mps.second[23]);
  STATIC_REQUIRE(mps.second[24]);
  STATIC_REQUIRE(mps.second[25]);
  STATIC_REQUIRE(mps.second[26]);
  STATIC_REQUIRE(mps.second[27]);
  STATIC_REQUIRE(mps.second[28]);
  STATIC_REQUIRE(mps.second[29]);
  STATIC_REQUIRE(mps.second[30]);
  STATIC_REQUIRE(mps.second[31]);
  STATIC_REQUIRE(mps.second[32]);
  STATIC_REQUIRE(mps.second[33]);
  STATIC_REQUIRE(mps.second[34]);
  STATIC_REQUIRE(mps.second[35]);
  STATIC_REQUIRE(mps.second[36]);
  STATIC_REQUIRE(mps.second[37]);
  STATIC_REQUIRE(mps.second[38]);
  STATIC_REQUIRE(mps.second[39]);
  STATIC_REQUIRE(mps.second[40]);
  STATIC_REQUIRE(mps.second[41]);
  STATIC_REQUIRE(mps.second[42]);
  STATIC_REQUIRE(mps.second[43]);
  STATIC_REQUIRE(mps.second[44]);
  STATIC_REQUIRE(mps.second[45]);
  STATIC_REQUIRE(mps.second[46]);
  STATIC_REQUIRE(mps.second[47]);
  STATIC_REQUIRE(mps.second[48]);
  STATIC_REQUIRE(mps.second[49]);
  STATIC_REQUIRE(mps.second[50]);
  STATIC_REQUIRE(mps.second[51]);
  STATIC_REQUIRE(mps.second[52]);
  STATIC_REQUIRE(mps.second[53]);
  STATIC_REQUIRE(mps.second[54]);
  STATIC_REQUIRE(mps.second[55]);
  STATIC_REQUIRE(mps.second[56]);
  STATIC_REQUIRE(mps.second[57]);
  STATIC_REQUIRE(mps.second[58]);
  STATIC_REQUIRE(mps.second[59]);
  STATIC_REQUIRE(mps.second[60]);
  STATIC_REQUIRE(mps.second[61]);
  STATIC_REQUIRE(mps.second[62]);
  STATIC_REQUIRE(mps.second[63]);
  STATIC_REQUIRE(mps.second[64]);
  STATIC_REQUIRE(mps.second[65]);
  STATIC_REQUIRE(mps.second[66]);
  STATIC_REQUIRE(mps.second[67]);
  STATIC_REQUIRE(mps.second[68]);
  STATIC_REQUIRE(mps.second[69]);
  STATIC_REQUIRE(mps.second[70]);
  STATIC_REQUIRE(mps.second[71]);
  STATIC_REQUIRE(mps.second[72]);
  STATIC_REQUIRE(mps.second[73]);
  STATIC_REQUIRE(mps.second[74]);
  STATIC_REQUIRE(mps.second[75]);
  STATIC_REQUIRE(mps.second[76]);
  STATIC_REQUIRE(mps.second[77]);
  STATIC_REQUIRE(mps.second[78]);
  STATIC_REQUIRE(mps.second[79]);
  STATIC_REQUIRE(mps.second[80]);
  STATIC_REQUIRE(mps.second[81]);
  STATIC_REQUIRE(mps.second[82]);
  STATIC_REQUIRE(mps.second[83]);
  STATIC_REQUIRE(mps.second[84]);
  STATIC_REQUIRE(mps.second[85]);
  STATIC_REQUIRE(mps.second[86]);
  STATIC_REQUIRE(mps.second[87]);
  STATIC_REQUIRE(mps.second[88]);
  STATIC_REQUIRE(mps.second[89]);
  STATIC_REQUIRE(mps.second[90]);
  STATIC_REQUIRE(mps.second[91]);
  STATIC_REQUIRE(mps.second[92]);
  STATIC_REQUIRE(mps.second[93]);
}

template<std::size_t N>
[[nodiscard]] constexpr auto TestProgramStatements(mpp::LexerState lexerState, std::array<mpp::ast::Statement, N> comps)
{
  std::array<bool, N> cmps{ false };
  auto parser = mpp::Parser(lexerState);
  auto prog = parser.ParseProgram();

  for (auto [result, statement, expected] : std::views::zip(cmps, prog.statements, comps))
  {
    result = statement == expected;
  }
  return std::make_pair(prog.statements.size(), cmps);
}

template<std::size_t N>
[[nodiscard]] auto TestProgramStatementsPrint(mpp::LexerState lexerState, const std::array<mpp::ast::Statement, N> &comps)
{
  std::array<bool, N> cmps{ false };
  auto parser = mpp::Parser(lexerState);
  auto prog = parser.ParseProgram();

  for (const auto &err : parser.errors) { WARN(err.error); }
  for (auto [result, statement, expected] : std::views::zip(cmps, prog.statements, comps)) {
    result = statement == expected;
    if (!result)
    {
      WARN(statement.String());
      WARN(expected.String());
    }
  }
  return std::make_pair(prog.statements.size(), cmps);
}


TEST_CASE("Parser correctly parses let statements", "[parser, LetStatement]")
{
  static constexpr std::string_view input = R"(
    let x = 5;
    let y = 10;
    let foobar = 838383;
  )";
  static constexpr int five = 5;
  static constexpr int ten = 10;
  static constexpr int biggy = 838383;

  static constexpr auto init_state = mpp::Lexer::MakeState(input);
  static constexpr auto mps = TestProgramStatements(init_state,
     std::to_array<mpp::ast::Statement>({ 
       mpp::ast::LetStatement{ mpp::ast::Identifier{ "x",{ mpp::TokenType::Ident, "x" } }, mpp::ast::Expression{ mpp::ast::IntegerLiteral{five,{ mpp::TokenType::Int, "5" }}}},
       mpp::ast::LetStatement{ mpp::ast::Identifier{ "y",{ mpp::TokenType::Ident, "y" } }, mpp::ast::Expression{ mpp::ast::IntegerLiteral{ten ,{ mpp::TokenType::Int, "10"}}}},
       mpp::ast::LetStatement{ mpp::ast::Identifier{ "foobar" ,{ mpp::TokenType::Ident, "foobar" }}, mpp::ast::Expression{ mpp::ast::IntegerLiteral{ biggy,{ mpp::TokenType::Int, "838383" } }} } 
    })
  );
  STATIC_REQUIRE(mps.first == 3);
  STATIC_REQUIRE(mps.second[0]);
  STATIC_REQUIRE(mps.second[1]);
  STATIC_REQUIRE(mps.second[2]);
}

TEST_CASE("Parser correctly parses return statements", "[parser, ReturnStatement]")
{
  static constexpr std::string_view input = R"(
    return 5;
    return 10;
    return 993322;
    return Bob;
  )";
  static constexpr int five = 5;
  static constexpr int ten = 10;
  static constexpr int biggy = 993322;

  static constexpr auto init_state = mpp::Lexer::MakeState(input);
  static constexpr auto mps = TestProgramStatements(init_state,
    std::to_array<mpp::ast::Statement>({ 
      mpp::ast::ReturnStatement{ mpp::ast::Expression{mpp::ast::IntegerLiteral{five ,{ mpp::TokenType::Int, "5" }} }},
      mpp::ast::ReturnStatement{ mpp::ast::Expression{mpp::ast::IntegerLiteral{ten ,{ mpp::TokenType::Int, "10" }}}},
      mpp::ast::ReturnStatement{ mpp::ast::Expression{mpp::ast::IntegerLiteral{biggy ,{ mpp::TokenType::Int, "993322" }}}},
      mpp::ast::ReturnStatement{ mpp::ast::Expression{mpp::ast::Identifier{ "Bob",{ mpp::TokenType::Ident, "Bob" } }}}
    })
  );
  STATIC_REQUIRE(mps.first == 4);
  STATIC_REQUIRE(mps.second[0]);
  STATIC_REQUIRE(mps.second[1]);
  STATIC_REQUIRE(mps.second[2]);
  STATIC_REQUIRE(mps.second[3]);
}

TEST_CASE("Parser correctly parses ExpressionStatement", "[parser, ExpressionStatement]")
{
  static constexpr std::string_view input = R"(
    foobar;
  )";

  static constexpr auto init_state = mpp::Lexer::MakeState(input);
  static constexpr auto mps = TestProgramStatements(init_state,
    std::to_array<mpp::ast::Statement>({
        mpp::ast::ExpressionStatement{ mpp::ast::Expression{ mpp::ast::Identifier{ "foobar",{ mpp::TokenType::Ident, "foobar" }  }},{ mpp::TokenType::Ident, "foobar" } } 
      })
  );
  STATIC_REQUIRE(mps.first == 1);
  STATIC_REQUIRE(mps.second[0]);
}

constexpr auto test_Program_String(std::string_view against)
{
  mpp::ast::Program program;
  auto id2 = mpp::ast::Identifier{};
  id2.token = mpp::Token{ mpp::TokenType::Ident, "anotherVar" };
  id2.Value = "anotherVar";

  mpp::ast::LetStatement let{ { "myVar", mpp::Token{ mpp::TokenType::Ident, "myVar" } }, id2 };

  program.statements.emplace_back(let);
  return program.String() == against;
}

TEST_CASE("Parser correctly outputs program as string", "[parser, string]")
{
  static constexpr auto res = test_Program_String("let myVar = anotherVar;");

  STATIC_REQUIRE(res);

}

TEST_CASE("Parser correctly parses IntegerLiteral statements", "[parser, IntegerLiteral]")
{
  static constexpr std::string_view input = R"(
    5;
  )";
  static constexpr int val = 5;

  static constexpr auto init_state = mpp::Lexer::MakeState(input);
  static constexpr auto mps = TestProgramStatements(init_state,
    std::to_array<mpp::ast::Statement>({
      mpp::ast::ExpressionStatement{ mpp::ast::Expression{ mpp::ast::IntegerLiteral{ val, { mpp::TokenType::Int, "5" } }}, { mpp::TokenType::Int, "5" } }
    })
  );
  STATIC_REQUIRE(mps.first == 1);
  STATIC_REQUIRE(mps.second[0]);
}
TEST_CASE("Parser correctly parses StringLiteral Expressions", "[parser, StringLiteral]")
{
  static constexpr std::string_view input = R"(
    "Hello, World!";
  )";
  static constexpr std::string_view val = "Hello, World!";

  static constexpr auto init_state = mpp::Lexer::MakeState(input);
  static constexpr auto mps = TestProgramStatements(init_state,
    std::to_array<mpp::ast::Statement>({
      mpp::ast::ExpressionStatement{ mpp::ast::Expression{ mpp::ast::StringLiteral{ val, { mpp::TokenType::String, val } }}, { mpp::TokenType::String, val } }
    })
  );
  STATIC_REQUIRE(mps.first == 1);
  STATIC_REQUIRE(mps.second[0]);
}


constexpr auto test_PrefixExpressions()
{
  constexpr std::string_view input = R"(
    !true;
    !false;
    !5;
    -15;
  )";

  constexpr auto init_state = mpp::Lexer::MakeState(input);

  constexpr auto num1 = 5;
  constexpr auto num2 = 15;
  const auto expr1 = std::make_unique<mpp::ast::Expression>(mpp::ast::IntegerLiteral{ num1, { mpp::TokenType::Int, "5" } }) ;
  const auto expr2 = std::make_unique<mpp::ast::Expression>(mpp::ast::IntegerLiteral{ num2, { mpp::TokenType::Int, "15" } });
  const auto expr_true = std::make_unique<mpp::ast::Expression>(mpp::ast::BoolLiteral{ true , { mpp::TokenType::True, "true" }});
  const auto expr_false = std::make_unique<mpp::ast::Expression>(mpp::ast::BoolLiteral{ false , { mpp::TokenType::False, "false" }});
  const auto val = TestProgramStatements(init_state,
    std::to_array<mpp::ast::Statement>({ 
        mpp::ast::ExpressionStatement{ mpp::ast::PrefixExpression{ "!", expr_true.get(), { mpp::TokenType::True, "true" }  }, { mpp::TokenType::True, "true" } },
        mpp::ast::ExpressionStatement{ mpp::ast::PrefixExpression{ "!", expr_false.get(), { mpp::TokenType::False, "false" }}, { mpp::TokenType::False, "false" } },
        mpp::ast::ExpressionStatement{ mpp::ast::PrefixExpression{ "!", expr1.get(),{ mpp::TokenType::Int, "5" }}, { mpp::TokenType::Int, "5" } },
        mpp::ast::ExpressionStatement{ mpp::ast::PrefixExpression{ "-", expr2.get(), { mpp::TokenType::Int, "15" } }, { mpp::TokenType::Int, "15" } }
      })
  );
  return val;
}

TEST_CASE("Parser correctly parses PrefixExpression", "[parser, PrefixExpression]")
{
  static constexpr auto mps = test_PrefixExpressions();
  
  STATIC_REQUIRE(mps.first == 4);
  STATIC_REQUIRE(mps.second[0]);
  STATIC_REQUIRE(mps.second[1]);
  STATIC_REQUIRE(mps.second[3]);
  STATIC_REQUIRE(mps.second[2]);
}

 constexpr auto test_InfixExpressions()
{
  constexpr std::string_view input = R"(
    5 + 5;
    5 - 5;
    5 * 5;
    5 / 5;
    5 > 5;
    5 < 5;
    5 == 5;
    5 != 5;
    true == true;
    true != false;
    false == false;
  )";

  constexpr auto init_state = mpp::Lexer::MakeState(input);

  constexpr auto num = 5;
  constexpr auto token_int_5 = mpp::Token{ mpp::TokenType::Int, "5" };
  constexpr auto token_bool_true = mpp::Token{ mpp::TokenType::True, "true" };
  constexpr auto token_bool_false = mpp::Token{ mpp::TokenType::False, "false" };

  const auto expr = std::make_unique<mpp::ast::Expression>(mpp::ast::IntegerLiteral{ num, token_int_5 });
  const auto expr_true = std::make_unique<mpp::ast::Expression>(mpp::ast::BoolLiteral{ true, token_bool_true });
  const auto expr_false = std::make_unique<mpp::ast::Expression>(mpp::ast::BoolLiteral{ false, token_bool_false });

  const auto val = TestProgramStatements(init_state,
    std::to_array<mpp::ast::Statement>( {
        mpp::ast::ExpressionStatement{ mpp::ast::InfixExpression{ expr.get(), "+", expr.get(), token_int_5 },token_int_5 },
        mpp::ast::ExpressionStatement{ mpp::ast::InfixExpression{ expr.get(), "-", expr.get(), token_int_5 },token_int_5 },
        mpp::ast::ExpressionStatement{ mpp::ast::InfixExpression{ expr.get(), "*", expr.get(), token_int_5 },token_int_5 },
        mpp::ast::ExpressionStatement{ mpp::ast::InfixExpression{ expr.get(), "/", expr.get(), token_int_5 },token_int_5 },
        mpp::ast::ExpressionStatement{ mpp::ast::InfixExpression{ expr.get(), ">", expr.get(), token_int_5 },token_int_5 },
        mpp::ast::ExpressionStatement{ mpp::ast::InfixExpression{ expr.get(), "<", expr.get(), token_int_5 },token_int_5 },
        mpp::ast::ExpressionStatement{ mpp::ast::InfixExpression{ expr.get(), "==", expr.get(), token_int_5 },token_int_5 },
        mpp::ast::ExpressionStatement{ mpp::ast::InfixExpression{ expr.get(), "!=", expr.get(), token_int_5 },token_int_5 },
        mpp::ast::ExpressionStatement{ mpp::ast::InfixExpression{ expr_true.get(), "==", expr_true.get(), token_bool_true }, token_bool_true },
        mpp::ast::ExpressionStatement{ mpp::ast::InfixExpression{ expr_true.get(), "!=", expr_false.get(),token_bool_false }, token_bool_false },
        mpp::ast::ExpressionStatement{ mpp::ast::InfixExpression{ expr_false.get(), "==", expr_false.get(), token_bool_false }, token_bool_false }
    })
  );
  return val;
}

TEST_CASE("Parser correctly parses InfixExpression", "[parser, InfixExpression]")
{
  static constexpr auto mps = test_InfixExpressions();

  STATIC_REQUIRE(mps.first == 11);
  STATIC_REQUIRE(mps.second[0]);
  STATIC_REQUIRE(mps.second[1]);
  STATIC_REQUIRE(mps.second[2]);
  STATIC_REQUIRE(mps.second[3]);
  STATIC_REQUIRE(mps.second[4]);
  STATIC_REQUIRE(mps.second[5]);
  STATIC_REQUIRE(mps.second[6]);
  STATIC_REQUIRE(mps.second[7]);
  STATIC_REQUIRE(mps.second[8]);
  STATIC_REQUIRE(mps.second[9]);
  STATIC_REQUIRE(mps.second[10]);
}

constexpr bool test_Program_String_Output(std::string_view input,const std::string& against)
{
  auto init_state = mpp::Lexer::MakeState(input);
  auto parser = mpp::Parser(init_state);
  auto program = parser.ParseProgram();
  return program.String() == against;
}

bool test_Program_String_Output_PrintErrors(std::string_view input, const std::string &against)
{
  auto init_state = mpp::Lexer::MakeState(input);
  auto parser = mpp::Parser(init_state);
  auto program = parser.ParseProgram();
  for (const auto& err : parser.errors) 
  {
    WARN(err.position);
    WARN(err.error);
  }
  WARN(program.String());
  return program.String() == against;
}

TEST_CASE("Parser correctly parses and groups InfixExpression to String", "[parser, InfixExpression]")
{
  STATIC_REQUIRE(test_Program_String_Output( "-a * b", "((-a) * b)" ));
  STATIC_REQUIRE(test_Program_String_Output( "!-a", "(!(-a))" ));
  STATIC_REQUIRE(test_Program_String_Output( "-a + b", "((-a) + b)" ));
  STATIC_REQUIRE(test_Program_String_Output( "a + b + c", "((a + b) + c)" ));
  STATIC_REQUIRE(test_Program_String_Output( "a + b - c", "((a + b) - c)" ));
  STATIC_REQUIRE(test_Program_String_Output( "a * b * c", "((a * b) * c)" ));
  STATIC_REQUIRE(test_Program_String_Output( "a * b / c", "((a * b) / c)" ));
  STATIC_REQUIRE(test_Program_String_Output( "a + b / c", "(a + (b / c))" ));
  STATIC_REQUIRE(test_Program_String_Output( "a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)" ));
  STATIC_REQUIRE(test_Program_String_Output( "3 + 4; -5 * 5", "(3 + 4)((-5) * 5)" ));
  STATIC_REQUIRE(test_Program_String_Output( "5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))" ));
  STATIC_REQUIRE(test_Program_String_Output( "5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))" ));
  STATIC_REQUIRE(test_Program_String_Output( "3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" ));
  STATIC_REQUIRE(test_Program_String_Output( "3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" ));
}

TEST_CASE("Parser correctly parses and groups BoolLiteral to String", "[parser, BoolLiteral]")
{
  STATIC_REQUIRE(test_Program_String_Output("true", "true"));
  STATIC_REQUIRE(test_Program_String_Output("false", "false"));
  STATIC_REQUIRE(test_Program_String_Output("3 > 5 == false", "((3 > 5) == false)"));
  STATIC_REQUIRE(test_Program_String_Output("3 < 5 == true", "((3 < 5) == true)"));
}

TEST_CASE("Parser correctly parses and groups with parseGroupedExpression to String", "[parser, parseGroupedExpression]")
{
  STATIC_REQUIRE(test_Program_String_Output("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"));
  STATIC_REQUIRE(test_Program_String_Output("(5 + 5) * 2", "((5 + 5) * 2)"));
  STATIC_REQUIRE(test_Program_String_Output("2 / (5 + 5)", "(2 / (5 + 5))"));
  STATIC_REQUIRE(test_Program_String_Output("-(5 + 5)", "(-(5 + 5))"));
  STATIC_REQUIRE(test_Program_String_Output("!(true == true)", "(!(true == true))"));
}

TEST_CASE("Parser correctly parses IfStatement to String", "[parser, IfStatement]")
{
  STATIC_REQUIRE(test_Program_String_Output("if (x < y) {x}", "if(x < y){\nx\n}"));
  STATIC_REQUIRE(test_Program_String_Output("if (x < y) {x} else { y }", "if(x < y){\nx\n}\nelse\n{\ny\n}"));
  // TODO: ensure this fails: "if(){1}else{0};"
}

TEST_CASE("Parser correctly parses FunctionLiteral to String", "[parser, FunctionLiteral]")
{
  STATIC_REQUIRE(test_Program_String_Output("fn(){}", "fn(){\n}"));
  STATIC_REQUIRE(test_Program_String_Output("fn(x){}", "fn(x){\n}"));
  STATIC_REQUIRE(test_Program_String_Output("fn(x, y, z){}", "fn(x, y, z){\n}"));
}

TEST_CASE("Parser correctly parses CallExpression to String", "[parser, CallExpression]")
{
  STATIC_REQUIRE(test_Program_String_Output("add()", "add()"));
  STATIC_REQUIRE(test_Program_String_Output("add(1,2)", "add(1, 2)"));
  STATIC_REQUIRE(test_Program_String_Output("add(5+5)", "add((5 + 5))"));
  STATIC_REQUIRE(test_Program_String_Output("add(add(1,2), a(b,c)+1)", "add(add(1, 2), (a(b, c) + 1))"));

  STATIC_REQUIRE(test_Program_String_Output("a + add(b * c) + d", "((a + add((b * c))) + d)"));
  STATIC_REQUIRE(test_Program_String_Output("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"));
  STATIC_REQUIRE(test_Program_String_Output("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"));
}

TEST_CASE("Parser correctly parses LetStatement to String", "[parser, LetStatement]")
{
  STATIC_REQUIRE(test_Program_String_Output("let x = 5;", "let x = 5;"));
  STATIC_REQUIRE(test_Program_String_Output("let x = true;", "let x = true;"));
  STATIC_REQUIRE(test_Program_String_Output("let x = foobar;", "let x = foobar;"));

  STATIC_REQUIRE(test_Program_String_Output("let x = 5*5;", "let x = (5 * 5);"));
  STATIC_REQUIRE(test_Program_String_Output("let x = add(4, 3*3);", "let x = add(4, (3 * 3));"));
}

TEST_CASE("Parser correctly parses ReturnStatement to String", "[parser, ReturnStatement]")
{
  STATIC_REQUIRE(test_Program_String_Output("return 5;", "return 5;"));
  STATIC_REQUIRE(test_Program_String_Output("return true;", "return true;"));
  STATIC_REQUIRE(test_Program_String_Output("return foobar;", "return foobar;"));

  STATIC_REQUIRE(test_Program_String_Output("return 5*5;", "return (5 * 5);"));
  STATIC_REQUIRE(test_Program_String_Output("return add(4, 3*3);", "return add(4, (3 * 3));"));
}

TEST_CASE("Parser correctly parses ArrayLiteral to String", "[parser, ArrayLiteral]")
{
  STATIC_REQUIRE(test_Program_String_Output("[1,x,foo(y),\"bob\", 2 * 3]", "[1, x, foo(y), \"bob\", (2 * 3)]"));
  STATIC_REQUIRE(test_Program_String_Output("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"));
  STATIC_REQUIRE(test_Program_String_Output("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"));
  STATIC_REQUIRE(test_Program_String_Output("[]", "[]"));
}

TEST_CASE("Parser correctly parses HashLiteral to String", "[parser, HashLiteral]")
{
  STATIC_REQUIRE(test_Program_String_Output("{1:2, 3:4, 5:6}", "{1:2, 3:4, 5:6}"));
  STATIC_REQUIRE(test_Program_String_Output("add(a * b[2], b[1], 2 * {1:2}[1])", "add((a * (b[2])), (b[1]), (2 * ({1:2}[1])))"));
  STATIC_REQUIRE(test_Program_String_Output("a * {1:2, 3:4, \"4\":5}[b * c] * d", "((a * ({1:2, 3:4, \"4\":5}[(b * c)])) * d)"));
  STATIC_REQUIRE(test_Program_String_Output("{true:1, false:0}", "{true:1, false:0}"));
  STATIC_REQUIRE(test_Program_String_Output("{}", "{}"));

  
  // REQUIRE(test_Program_String_Output_PrintErrors("{1:2, 3:4, 5:6}", "{1:2, 3:4, 5:6}"));
  // REQUIRE(test_Program_String_Output_PrintErrors("add(a * b[2], b[1], 2 * {1:2}[1])", "add((a * (b[2])), (b[1]), (2 * ({1:2}[1])))"));
  // REQUIRE(test_Program_String_Output_PrintErrors("a * {1:2, 3:4, \"4\":5}[b * c] * d", "((a * ([1:2, 3:4, \"4\":5][(b * c)])) * d)"));
  // REQUIRE(test_Program_String_Output_PrintErrors("{true:1, false:0}", "{true:1, false:0}"));
  // REQUIRE(test_Program_String_Output_PrintErrors("{}", "{}"));
}

TEST_CASE("Parser correctly parses IndexExpression to String", "[parser, IndexExpression]")
{
  STATIC_REQUIRE(test_Program_String_Output("[1,x,foo(y),\"bob\", 2 * 3][1]", "([1, x, foo(y), \"bob\", (2 * 3)][1])"));
  // we don't actually support multiple subscripts (yet)
  // STATIC_REQUIRE(test_Program_String_Output("A[1,x,foo(y),\"bob\", 2 * 3]", "(A[1, x, foo(y), \"bob\", (2 * 3)])"));
  STATIC_REQUIRE(test_Program_String_Output("B[0]", "(B[0])")); 
  STATIC_REQUIRE(test_Program_String_Output("B[3*3]", "(B[(3 * 3)])")); 
}
