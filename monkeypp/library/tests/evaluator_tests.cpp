
#include <catch2/catch_message.hpp>
#include <catch2/catch_test_macros.hpp>

#include "monkeypp.hpp"
#include <string>
#include <string_view>
#include <iostream>
#include <sstream>

mpp::Object::Object TestEvalTo(std::string_view input)
{ 
  const auto lex = mpp::Lexer::MakeState(input);
  auto parser = mpp::Parser{ lex };
  auto program = parser.ParseProgram();
  return mpp::Evaluator{}.EvaluateProgram(program);
}

template <bool warn = false>
bool TestEvalTo(std::string_view input, auto other)
{
  const auto lex = mpp::Lexer::MakeState(input);
  auto parser = mpp::Parser{ lex };
  auto program = parser.ParseProgram();

  auto res = mpp::Evaluator{}.EvaluateProgram(program); 
  auto ret = res == other;

  if constexpr (warn)
  {
    if (!ret) {
      WARN(res.String());
      WARN(other.String());
    }
  }

  return ret;
}

constexpr bool TestEvalToAndPrints(std::string_view input, auto other, std::string_view output)
{
  const std::stringstream buffer;
  std::streambuf * old = std::cout.rdbuf(buffer.rdbuf());
  bool res = TestEvalTo(input, other);
  const std::string bufres = buffer.str();
  res &= bufres == output;
  std::cout.rdbuf(old);
  return res;
}

TEST_CASE("Evaluator correctly evaluates Integer Expressions", "[Evaluator, IntegerObject]")
{
  REQUIRE(TestEvalTo("6", mpp::Object::IntegerObject{ 6 }));
}

TEST_CASE("Evaluator correctly evaluates Boolean Expressions", "[Evaluator, BoolObject]")
{
  REQUIRE(TestEvalTo("true", mpp::Object::BoolObject{ true }));
  REQUIRE(TestEvalTo("false", mpp::Object::BoolObject{ false }));
}

TEST_CASE("Evaluator correctly evaluates PrefixExpressions", "[Evaluator, PrefixExpression]")
{
  REQUIRE(TestEvalTo("!false", mpp::Object::BoolObject{ true }));
  REQUIRE(TestEvalTo("!true", mpp::Object::BoolObject{ false }));
  REQUIRE(TestEvalTo("!5", mpp::Object::BoolObject{ false }));
  REQUIRE(TestEvalTo("!null", mpp::Object::BoolObject{ true }));
  REQUIRE(TestEvalTo("!!true", mpp::Object::BoolObject{ true }));
  REQUIRE(TestEvalTo("!!false", mpp::Object::BoolObject{ false }));
  REQUIRE(TestEvalTo("!!5", mpp::Object::BoolObject{ true }));
  REQUIRE(TestEvalTo("!!null", mpp::Object::BoolObject{ false }));

  REQUIRE(TestEvalTo("-5", mpp::Object::IntegerObject{ -5 }));
  REQUIRE(TestEvalTo("--5", mpp::Object::IntegerObject{ 5 }));

  REQUIRE(TestEvalTo("+5", mpp::Object::IntegerObject{ 5 }));
  REQUIRE(TestEvalTo("++5", mpp::Object::IntegerObject{ 5 }));
}

TEST_CASE("Evaluator correctly evaluates IntegerObject InfixExpression", "[Evaluator, IntegerObject, InfixExpression]")
{
  REQUIRE(TestEvalTo("5 + 5 + 5 + 5", mpp::Object::IntegerObject{ 20 }));
  REQUIRE(TestEvalTo("5 + 5 + 5 + 5 - 10", mpp::Object::IntegerObject{ 10 }));
  REQUIRE(TestEvalTo("2 * 2 * 2 * 2 * 2", mpp::Object::IntegerObject{ 32 }));
  REQUIRE(TestEvalTo("-50 + 100 + -50", mpp::Object::IntegerObject{ 0 }));
  REQUIRE(TestEvalTo("5 * 2 + 10", mpp::Object::IntegerObject{ 20 }));
  REQUIRE(TestEvalTo("5 + 2 * 10", mpp::Object::IntegerObject{ 25 }));
  REQUIRE(TestEvalTo("20 + 2 * -10", mpp::Object::IntegerObject{ 0 }));
  REQUIRE(TestEvalTo("50 / 2 * 2 + 10", mpp::Object::IntegerObject{ 60 }));
  REQUIRE(TestEvalTo("2 * (5 + 10)", mpp::Object::IntegerObject{ 30 }));
  REQUIRE(TestEvalTo("3 * 3 * 3 + 10", mpp::Object::IntegerObject{ 37 }));
  REQUIRE(TestEvalTo("3 * (3 * 3) + 10", mpp::Object::IntegerObject{ 37 }));
  REQUIRE(TestEvalTo("(5 + 10 * 2 + 15 / 3) * 2 + -10", mpp::Object::IntegerObject{ 50 }));
}

TEST_CASE("Evaluator correctly evaluates BoolObject InfixExpressions", "[Evaluator, BoolObject, InfixExpression]")
{
  REQUIRE(TestEvalTo("true == true", mpp::Object::BoolObject{ true}));
  REQUIRE(TestEvalTo("false == false", mpp::Object::BoolObject{ true}));
  REQUIRE(TestEvalTo("true == false", mpp::Object::BoolObject{ false}));
  REQUIRE(TestEvalTo("true != false", mpp::Object::BoolObject{ true}));
  REQUIRE(TestEvalTo("false != true", mpp::Object::BoolObject{ true}));
  REQUIRE(TestEvalTo("(1 < 2) == true", mpp::Object::BoolObject{ true}));
  REQUIRE(TestEvalTo("(1 < 2) == false", mpp::Object::BoolObject{ false}));
  REQUIRE(TestEvalTo("(1 > 2) == true", mpp::Object::BoolObject{ false}));
  REQUIRE(TestEvalTo("(1 > 2) == false", mpp::Object::BoolObject{ true}));

  REQUIRE(TestEvalTo("10 == (5 + 5)", mpp::Object::BoolObject{ true}));
  REQUIRE(TestEvalTo("(100/20) == 5", mpp::Object::BoolObject{ true}));
  REQUIRE(TestEvalTo("(100/20) == 5", mpp::Object::BoolObject{ true}));
  REQUIRE(TestEvalTo("null < -1", mpp::Object::BoolObject{ true}));
  REQUIRE(TestEvalTo("-1 > null", mpp::Object::BoolObject{ true}));
  REQUIRE(TestEvalTo("null > 10000", mpp::Object::BoolObject{ false}));
  REQUIRE(TestEvalTo("10000 < null", mpp::Object::BoolObject{ false}));
  REQUIRE(TestEvalTo("null > null", mpp::Object::BoolObject{ false}));
  REQUIRE(TestEvalTo("null < null", mpp::Object::BoolObject{ false}));
  REQUIRE(TestEvalTo("null == null", mpp::Object::BoolObject{ true}));
  REQUIRE(TestEvalTo("null != null", mpp::Object::BoolObject{ false}));
  REQUIRE(TestEvalTo("null == 3", mpp::Object::BoolObject{ false}));
  REQUIRE(TestEvalTo("3 == null", mpp::Object::BoolObject{ false}));
  REQUIRE(TestEvalTo("3 != null", mpp::Object::BoolObject{ true}));
  REQUIRE(TestEvalTo("null != 3", mpp::Object::BoolObject{ true}));
}

TEST_CASE("Evaluator correctly evaluates IfExpressions", "[Evaluator, IfExpressions]")
{
  REQUIRE(TestEvalTo("if (true) { 10 }", mpp::Object::IntegerObject{ 10}));
  REQUIRE(TestEvalTo("if (false) { 10 }", mpp::Object::NullObject{}));
  REQUIRE(TestEvalTo("if (1) { 10 }", mpp::Object::IntegerObject{ 10}));
  REQUIRE(TestEvalTo("if (1 < 2) { 10 }", mpp::Object::IntegerObject{ 10}));
  REQUIRE(TestEvalTo("if (1 > 2) { 10 }", mpp::Object::NullObject{}));
  REQUIRE(TestEvalTo("if (1 > 2) { 10 } else { 20 }", mpp::Object::IntegerObject{ 20}));
  REQUIRE(TestEvalTo("if (1 < 2)  { 10 } else { 20 }", mpp::Object::IntegerObject{ 10}));
}

template<bool isConstexpr = true>
constexpr bool TestEvalReturnObjectTo(std::string_view input, auto other)
{
  const auto lex = mpp::Lexer::MakeState(input);
  auto parser = mpp::Parser{ lex };
  auto program = parser.ParseProgram();

  mpp::Evaluator eval{};
  auto res = eval.EvaluateProgram(program);
  auto rhs = mpp::Object::ReturnObject{ eval.getNewObject(other) };
  auto ret = res == *rhs.Value;

  if constexpr (!isConstexpr)
  {
    if (!ret)
    {
      //WARN(res.String());
      //WARN(rhs.Value->String());
    }
  }

  return ret;
}

TEST_CASE("Evaluator correctly evaluates ReturnStatement", "[Evaluator, ReturnStatement]")
{
  REQUIRE(TestEvalReturnObjectTo("return 10;", mpp::Object::IntegerObject{ 10 }));
  REQUIRE(TestEvalReturnObjectTo("return 10; 9;", mpp::Object::IntegerObject{ 10 }));
  REQUIRE(TestEvalReturnObjectTo("return 2*5; 9;", mpp::Object::IntegerObject{ 10 }));
  REQUIRE(TestEvalReturnObjectTo("9; return 2 * 5; 9;", mpp::Object::IntegerObject{ 10 }));
  // test nested return statements:
  REQUIRE(TestEvalReturnObjectTo("if (10 > 1) {if (10 > 1){ return 10;} return 1}", mpp::Object::IntegerObject{ 10 }));
}

template<bool isConstexpr = true>
constexpr bool TestEvalToError(std::string_view input, auto other)
{
  const auto lex = mpp::Lexer::MakeState(input);
  auto parser = mpp::Parser{ lex };
  auto program = parser.ParseProgram();

  mpp::Evaluator eval{};
  auto res = eval.EvaluateProgram(program);
  auto ret = res == mpp::Object::ErrorObject{ std::string{ other } };

  if constexpr (!isConstexpr)
  {
    if (!ret)
    {
      WARN(res.String());
      WARN(other);
    }
  }

  return ret;
}

TEST_CASE("Evaluator correctly evaluates ErrorObjects", "[Evaluator, ErrorObjects]")
{
  REQUIRE(TestEvalToError("-true;", "Unknown Operator: -bool(true)"));
  REQUIRE(TestEvalToError("+false;", "Unknown Operator: +bool(false)"));
  REQUIRE(TestEvalToError("true+false;", "Unknown Operator: bool(true)+bool(false)"));
  REQUIRE(TestEvalToError("true/false;", "Unknown Operator: bool(true)/bool(false)"));

  REQUIRE(TestEvalToError("5+false;", "Type Mismatch: int(5)+bool(false)"));
  REQUIRE(TestEvalToError("5/false;", "Type Mismatch: int(5)/bool(false)"));

  REQUIRE(TestEvalToError("if(5/false){1}else{0};", "Type Mismatch: int(5)/bool(false)"));
  //REQUIRE(TestEvalToError("if(){1}else{0};", "IfExpression holds no Condition"));
  REQUIRE(TestEvalToError("if(true){}else{0};", "IfExpression holds no Consequence"));
  REQUIRE(TestEvalToError("if(10>1){if(10>1){return true+false;} return 1;}", "Unknown Operator: bool(true)+bool(false)"));
}

TEST_CASE("Evaluator correctly evaluates LetStatements", "[Evaluator, LetStatements]")
{
  REQUIRE(TestEvalTo("let a = 5; a;", mpp::Object::IntegerObject{ 5 }));
  REQUIRE(TestEvalTo("let a = 5; a+5;", mpp::Object::IntegerObject{ 10 }));
  REQUIRE(TestEvalTo("let a = 5*5; a;", mpp::Object::IntegerObject{ 25 }));
  REQUIRE(TestEvalTo("let a = 5; let b = a; b;", mpp::Object::IntegerObject{ 5 }));
  REQUIRE(TestEvalTo("let a = 5; let b = a; let c = a + b + 5; c;", mpp::Object::IntegerObject{ 15 }));
  REQUIRE(TestEvalToError("foobar", "Identifier Not Found: foobar"));
}

TEST_CASE("Evaluator correctly evaluates FunctionLiteral", "[Evaluator, FunctionLiteral]")
{
  REQUIRE(TestEvalTo("let identity = fn(x) { x; }; identity(5);", mpp::Object::IntegerObject{ 5}));
  REQUIRE(TestEvalTo("let identity = fn(x) { return x; }; identity(5);", mpp::Object::IntegerObject{ 5}));
  REQUIRE(TestEvalTo("let double = fn(x) { x * 2; }; double(5);", mpp::Object::IntegerObject{ 10}));
  REQUIRE(TestEvalTo("let add = fn(x, y) { x + y; }; add(5, 5);", mpp::Object::IntegerObject{ 10}));
  REQUIRE(TestEvalTo("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", mpp::Object::IntegerObject{ 20}));
  REQUIRE(TestEvalTo("fn(x) { x; }(5)", mpp::Object::IntegerObject{ 5}));
  REQUIRE(TestEvalTo("fn(x) { return x-5; }(5)", mpp::Object::IntegerObject{0}));

  // test nesed functions:
  REQUIRE(TestEvalTo("fn(a){fn(b){a+b}}(5)(5)", mpp::Object::IntegerObject{ 10}));
  REQUIRE(TestEvalTo("let x = fn(a){fn(b){a+b}};x(x(5)(5))(x(5)(50))", mpp::Object::IntegerObject{ 65}));
  REQUIRE(TestEvalTo("let x = fn(a){return fn(b){return a+b;};};x(x(5)(5))(x(5)(50))", mpp::Object::IntegerObject{ 65}));
}

TEST_CASE("Evaluator correctly evaluates StringLiteral", "[Evaluator, StringLiteral]")
{
  // TODO: update to clang 18 so that these can use STATIC_REQUIRE
  REQUIRE(TestEvalTo("let str = \"Hello, World!\"", mpp::Object::StringObject{"Hello, World!"}));
  REQUIRE(TestEvalTo("let str = \"Hello\"+\", World!\"", mpp::Object::StringObject{"Hello, World!"}));
  REQUIRE(TestEvalTo("let str = \"Hello\"; let str2=\", World!\"; str+str2;", mpp::Object::StringObject{"Hello, World!"}));
}

TEST_CASE("Evaluator correctly evaluates ArrayObject", "[Evaluator, ArrayObject]")
{
  const auto one = mpp::Object::IntegerObject{1};
  const auto one_one_one = mpp::Object::IntegerObject{111};
  const auto three = mpp::Object::IntegerObject{3};
  const auto four = mpp::Object::IntegerObject{4};
  const auto six = mpp::Object::IntegerObject{6};
  const auto null = mpp::Object::NullObject{};

  REQUIRE(TestEvalTo("[1, 2*2, 3+3];", mpp::Object::ArrayObject{{ one, four, six }}));
  REQUIRE(TestEvalTo("[1, 2*2, 3+3][0];", one));
  REQUIRE(TestEvalTo("[1, 2*2, 3+3][1];", four));
  REQUIRE(TestEvalTo("[1, 2*2, 3+3][2];", six));
  REQUIRE(TestEvalTo("let x = [1, 2, 3]; x[2];", three));
  REQUIRE(TestEvalTo("fn(){[0,1,2];}()[1]", one));
  REQUIRE(TestEvalTo("fn(){return [0,1,2];}()[1]", one));
  REQUIRE(TestEvalTo("fn(){return [0,1,fn(x){x+100}];}()[2](11)", one_one_one));
  REQUIRE(TestEvalTo("[1, 2*2, 3+3][-1];", null));
  REQUIRE(TestEvalTo("[1, 2*2, 3+3][6];", null));
}

TEST_CASE("Evaluator correctly evaluates HashObject", "[Evaluator, HashObject]")
{
  const auto one = mpp::Object::IntegerObject{1};
  const auto one_one_one = mpp::Object::IntegerObject{111};
  const auto two = mpp::Object::IntegerObject{2};
  const auto four = mpp::Object::IntegerObject{4};
  const auto six = mpp::Object::IntegerObject{6};
  const auto False = mpp::Object::BoolObject{false};
  const auto True = mpp::Object::BoolObject{true};
  const auto test = mpp::Object::StringObject{"test1"};
  const auto test2 = mpp::Object::StringObject{"test2"};
  const auto hash1 = mpp::Object::HashObject{{
      {one.Hash(), {one,two} },
      {four.Hash(), {four,test} },
      {test2.Hash(), {test2,False} },
      {True.Hash(), {True,six} }
  }};
  REQUIRE(TestEvalTo(R"({1:2, 4:"test1", "test2":false, true:6})",hash1));
 
  REQUIRE(TestEvalTo(R"({1:2, 4:"test1", "test2":false, true:6}[1])",two));
  REQUIRE(TestEvalTo(R"({1:2, 4:"test1", "test2":false, true:6}[4])",test));
  REQUIRE(TestEvalTo(R"({1:2, 4:"test1", "test2":false, true:6}["test2"])",False));
  REQUIRE(TestEvalTo(R"({1:2, 4:"test1", "test2":false, true:6}[true])",six));
  REQUIRE(TestEvalTo(R"(let x = {1:2, 4:"test1", "test2":false, true:6};x[true];)",six));
  
  REQUIRE(TestEvalTo(R"(fn(){{1:2, 4:"test1", "test2":false, true:6};}()[true])",six));
  REQUIRE(TestEvalTo(R"(fn(){return {1:2, 4:"test1", "test2":false, true:6};}()[true])",six));
  REQUIRE(TestEvalTo(R"(fn(){return {1:2, 4:"test1", "test2":false, true:fn(x){x+100;}};}()[true](11))",one_one_one));
  REQUIRE(TestEvalTo(R"({1:2, 4:"test1", "test2":false, true:6}["DNE"])",mpp::Object::NullObject{}));

}

TEST_CASE("Evaluator correctly evaluates Builtin FunctionLiteral", "[Evaluator, Builtin FunctionLiteral]")
{
  const auto one = mpp::Object::IntegerObject{1};
  const auto two = mpp::Object::IntegerObject{2};
  const auto five = mpp::Object::IntegerObject{5};
  const auto thirteen = mpp::Object::IntegerObject{13};
  REQUIRE(TestEvalTo("len([1,2,3,4,5]);", five));
  REQUIRE(TestEvalTo("first([1,2,3,4,5]);", one));
  REQUIRE(TestEvalTo("last([1,2,3,4,5]);", five));
  REQUIRE(TestEvalTo("rest([1,2]);", mpp::Object::ArrayObject{{two}}));
  REQUIRE(TestEvalTo("push([1],2);", mpp::Object::ArrayObject{{one, two}}));
  REQUIRE(TestEvalTo("len(\"12345\");", five));
  REQUIRE(TestEvalTo("first(\"12345\");", mpp::Object::StringObject{"1"}));
  REQUIRE(TestEvalTo("last(\"12345\");", mpp::Object::StringObject{"5"}));
  REQUIRE(TestEvalTo("rest(\"12345\");", mpp::Object::StringObject{"2345"}));
  REQUIRE(TestEvalTo("push(\"12345\",\"99\");", mpp::Object::StringObject{"1234599"}));
  REQUIRE(TestEvalTo("let str = \"Hello\"; let str2=\", World!\"; len(str+str2);", thirteen));

  auto printRes = (TestEvalToAndPrints(R"(puts(1,2," ","hello!", " world")))", mpp::Object::NullObject{}, "12 hello! world\n"));
  REQUIRE(printRes);
}

TEST_CASE("Evaluator correctly evaluates Builtin Object", "[Evaluator, Builtin Object]")
{
  REQUIRE(TestEvalTo("null", mpp::Object::NullObject{}));
}

TEST_CASE("Evaluator correctly evaluates complex higher-order functions", "[Evaluator, complex eval]")
{
  REQUIRE(TestEvalTo(R"(
  let map = fn(arr, f){ let iter = fn(arr, accumulated){ if(len(arr) == 0) { accumulated } else { iter(rest(arr), push(accumulated, f(first(arr)))); } }; iter(arr, []); }
  let a = [1, 2, 3];
  let double = fn(x){x*2};
  map(a, double);
  )", mpp::Object::ArrayObject{{mpp::Object::IntegerObject{2}, mpp::Object::IntegerObject{4}, mpp::Object::IntegerObject{6}}}));

  REQUIRE(TestEvalTo(R"(
  let reduce = fn(arr, initial, f) {
    let iter = fn(arr, result) {
      if (len(arr) == 0) {
        result
      } else {
        iter(rest(arr), f(result, first(arr)));
      }
    };
    iter(arr, initial);
  };
  let sum = fn(arr){ reduce(arr, 0, fn(init, el){init+el})}
  sum([1,2,3,4,5])
  )", mpp::Object::IntegerObject{15}));
}

