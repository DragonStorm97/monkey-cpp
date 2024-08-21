// The main include files for this library monkeypp
#pragma once

#include <cstddef>
#include <iostream>
#include <iterator>
#include <string_view>
#include <array>
#include <unordered_map>
#include <utility>
#include <optional>
#include <vector>
#include <format>
#include <variant>
#include <cstdint>
#include <memory>
#include <ranges>
#include <string>
#include <functional>

namespace mpp::Object
{
  struct HashKey
  {
    std::string_view Type;
    std::uint64_t Value;
    [[nodiscard]] constexpr bool operator==(const HashKey&) const = default;
  };
}

template<>
struct std::hash<mpp::Object::HashKey>
{
  std::size_t operator()(const mpp::Object::HashKey& x) const
  {
    const auto seed = static_cast<std::size_t>(static_cast<unsigned char>(x.Type[0]));
    constexpr auto magic = 0x9e3779b9;
    constexpr auto magic6 = 6U;
    constexpr auto magic2 = 2U;
    return x.Value + magic + (seed << magic6) + (seed >> magic2);
  }
};

namespace mpp
{

  // TODO: std::string_view concat type (compile-type strings are safe to be in a string view, would be nice to concat them at comp-time too)

  // TODO: make tokens into structs..., then they can have their prefix and infix function defs, etc. inside them
  // TODO: then switch(curToken.type) can be changed to an std::visit (check performance)

  enum TokenType : std::uint8_t
  {
    Illegal = 127,
    Assign = 0,
    Asterisk,
    Bang,
    Comma,
    Colon,
    Else,
    Eof,
    False,
    Function,
    GreaterThan,
    Ident,
    If,
    Int,
    LBrace,
    LBracket,
    LessThan,
    Let,
    LParen,
    Minus,
    Plus,
    RBrace,
    RBracket,
    Return,
    RParen,
    Semicolon,
    Slash,
    String,
    True,
    CompEqual,
    CompNotEqual,
    Null
  };
  
  enum class Precedence : std::uint8_t 
  {
    LOWEST = 0,
    EQUALS,// ==
    LESSGREATER,// > or <
    SUM,// +
    PRODUCT,// *
    PREFIX,// -X or !X
    CALL,// myFunction(X)
    INDEX// array[index]
  };

  template<class Key, class Value, std::size_t Size>
  class CTMap
  {
  public:
  
    std::array<std::pair<Key, Value>, Size> pairs;

    [[nodiscard]] constexpr const Value* operator[](Key inKey) const
    {
      for (const auto &[key, value] : pairs)
      {
        if (key == inKey)
        {
          return &value;
        }
      }

      return nullptr;
    }

    [[nodiscard]] constexpr std::optional<Key> FindValue(const Value &val) const
    {
      for (const auto &[key, value] : pairs) {
        if (value == val) { return key; }
      }

      return {};
    }

    template<std::size_t idx>
    [[nodiscard]] constexpr Key GetKeyAt() const
    { 
      static_assert(idx >= 0 && idx <= Size);
      return pairs[idx].first;
    }

    template<std::size_t idx>
    [[nodiscard]] constexpr Value GetValueAt() const
    { 
      static_assert(idx >= 0 && idx <= Size);
      return pairs[idx].second;
    }
  };

  template<typename T>
  concept underlying_type_is_uint = std::unsigned_integral<std::underlying_type_t<T>>;// || std::is_enum_v<T>;

  template<underlying_type_is_uint Key, class Value, std::size_t Size> 
  class CTMap<Key, Value, Size>
  {
  public:
    std::array<Value, Size> pairs{};

    consteval explicit CTMap(const std::array<std::pair<Key, Value>, Size> &lst)
    { 
      for (auto [key, value] : lst) { pairs.at(key) = value; }
    }

    [[nodiscard]] constexpr const Value *operator[](Key key) const
    {
      return &pairs.at(key);
    }

    [[nodiscard]] constexpr std::optional<Key> FindValue(const Value &val) const
    {
      
      for (decltype(pairs.size()) idx = 0; idx < pairs.size(); ++idx) {
        if (pairs.at(idx) == val) { return static_cast<Key>(idx); }
      }
      return {};
    }

    template<Key idx> [[nodiscard]] constexpr Key GetKeyAt() const
    {
      static_assert(idx >= 0 && idx <= Size);
      return idx;
    }

    template<Key idx> [[nodiscard]] constexpr Value GetValueAt() const
    {
      static_assert(idx >= 0 && idx <= Size);
      return pairs[idx];
    }
  };
  

  namespace lookup {
    constexpr static auto symbols = CTMap<TokenType, std::string_view, 31>{
      {{ 
        { TokenType::Assign, "=" },
        { TokenType::Asterisk, "*" }, 
        { TokenType::Bang, "!" },
        { TokenType::Comma, "," },
        { TokenType::Colon, ":" },
        { TokenType::Else, "else" },
        { TokenType::Eof, "" },
        { TokenType::False, "false" },
        { TokenType::Function, "fn" },
        { TokenType::GreaterThan, ">" },
        { TokenType::Ident, "Identifier" },
        { TokenType::If, "if" },
        { TokenType::Int, "int" },
        { TokenType::LBrace, "{" },
        { TokenType::LBracket, "[" },
        { TokenType::LessThan, "<" },
        { TokenType::Let, "let" },
        { TokenType::LParen, "(" },
        { TokenType::Minus, "-" },
        { TokenType::Plus, "+" },
        { TokenType::RBrace, "}" },
        { TokenType::RBracket, "]" },
        { TokenType::Return, "return" },
        { TokenType::RParen, ")" },
        { TokenType::Semicolon, ";" },
        { TokenType::Slash, "/" },
        { TokenType::String, "string" },
        { TokenType::True, "true" },
        { TokenType::CompEqual, "==" },
        { TokenType::CompNotEqual, "!=" },
        { TokenType::Null, "null" },
      }}
    };

    [[nodiscard]] constexpr TokenType StringToken(std::string_view ident) noexcept
    {
      return symbols.FindValue(ident).value_or(TokenType::Ident);
    }

    [[nodiscard]] constexpr std::string_view TokenString(TokenType type) noexcept
    {
      if (const std::string_view *tok = symbols[type]; tok != nullptr) { return *tok; }
      return {};
    }

    // TODO: again, move this data with the token? (or derive it from definition order)
    [[nodiscard]] inline constexpr Precedence GetPrecedence(TokenType token)
    {
      switch (token) {
      case TokenType::LParen:
        return Precedence::CALL;
      case TokenType::LBracket:
        return Precedence::INDEX;
      case TokenType::Slash:
        [[fallthrough]];
      case TokenType::Asterisk:
        return Precedence::PRODUCT;
      case TokenType::GreaterThan:
        [[fallthrough]];
      case TokenType::LessThan:
        return Precedence::LESSGREATER;
      case TokenType::Minus:
        [[fallthrough]];
      case TokenType::Plus:
        return Precedence::SUM;
      case TokenType::CompEqual:
        [[fallthrough]];
      case TokenType::CompNotEqual:
        return Precedence::EQUALS;
      default:
        return Precedence::LOWEST;
      }
    }
  }

  struct Token
  {
    TokenType type = TokenType::Illegal;
    std::string_view literal;
    constexpr bool operator==(const Token &other) const = default;
    constexpr Token() = default;
    constexpr explicit Token(TokenType tokenType) : type(tokenType), literal(lookup::TokenString(tokenType)) {}
    constexpr Token(TokenType tokenType, std::string_view lit) : type(tokenType), literal(lit){};
  };

  [[nodiscard]] constexpr bool isLetter(char character) noexcept
  {
    return ('a' <= character && character <= 'z') || ('A' <= character && character <= 'Z') || character == '_';
  }

  [[nodiscard]] constexpr bool isDigit(char character) noexcept
  {
    return '0' <= character && character <= '9';
  }

  [[nodiscard]] constexpr bool isWhitespace(char character) noexcept
  {
    return character == ' ' || character == '\t' || character == '\n' || character == '\r';
  }

  struct LexerState
  {
    std::size_t position = 0;
    std::size_t readPosition = 0;
    char ch = '\0';
    char peekCh = '\0';

    std::string_view input;

    Token lastToken{};

    [[nodiscard]] constexpr char peekChar() const noexcept 
    {
      return readPosition >= input.length() ? '\0' : input[readPosition];
    }
  };

  namespace Lexer
  {
    // todo move the readX and skipX into some consumer lambda inside NextToken
    namespace internal 
    {

      [[nodiscard]] inline constexpr LexerState readChar(LexerState lexerState) noexcept
      {
        lexerState.ch = lexerState.peekChar();
        lexerState.position = lexerState.readPosition;
        ++lexerState.readPosition;
        lexerState.peekCh = lexerState.peekChar();
        return lexerState;
      }

      [[nodiscard]] inline constexpr LexerState readIdentifier(LexerState lexerState) noexcept
      {
        const std::size_t startPos = lexerState.position;
        while (isLetter(lexerState.ch) || isDigit(lexerState.ch)) { lexerState = readChar(lexerState); }
        lexerState.lastToken.literal = lexerState.input.substr(startPos, lexerState.position - startPos);
        lexerState.lastToken.type = lookup::StringToken(lexerState.lastToken.literal);
        return lexerState;
      }

      [[nodiscard]] inline constexpr LexerState readString(LexerState lexerState) noexcept
      {
        lexerState = readChar(lexerState); // consume the starting quote
        const std::size_t startPos = lexerState.position;
        while (lexerState.ch != '"' && lexerState.ch != 0) { lexerState = readChar(lexerState); }
        lexerState.lastToken.literal = lexerState.input.substr(startPos, lexerState.position - startPos);
        lexerState.lastToken.type = TokenType::String;
        lexerState = readChar(lexerState); // consume the end quote
        return lexerState;
      }

      [[nodiscard]] inline constexpr LexerState readNumber(LexerState lexerState) noexcept
      {
        const std::size_t startPos = lexerState.position;
        while (isDigit(lexerState.ch)) { lexerState = readChar(lexerState); }
        lexerState.lastToken.literal = lexerState.input.substr(startPos, lexerState.position - startPos);
        lexerState.lastToken.type = TokenType::Int;
        return lexerState;
      }

      [[nodiscard]] inline constexpr LexerState skipWhitespace(LexerState lexerState) noexcept
      {
        while (isWhitespace(lexerState.ch)) { lexerState = readChar(lexerState); }
        return lexerState;
      }
    }// namespace internal

    [[nodiscard]] inline constexpr LexerState MakeState(const std::string_view &input) noexcept
    { 
      LexerState state;
      state.input = input;
      return internal::readChar(state);
    }

    [[nodiscard]] inline constexpr LexerState NextToken(LexerState lexerState)
    {
      lexerState = internal::skipWhitespace(lexerState);
      const char peekCh = lexerState.peekCh;
      switch (lexerState.ch)
      {
        case '=':
        {
          if (peekCh == '=')
          {
            lexerState = internal::readChar(lexerState);
            lexerState.lastToken = Token{ TokenType::CompEqual };
          }
          else{
            lexerState.lastToken = Token{ TokenType::Assign };
          }
          break;
        }
        case ';':
          lexerState.lastToken = Token{ TokenType::Semicolon };
          break;
        case '(':
          lexerState.lastToken = Token{ TokenType::LParen };
          break;
        case ')':
          lexerState.lastToken = Token{ TokenType::RParen };
          break;
        case ',':
          lexerState.lastToken = Token{ TokenType::Comma };
          break;
        case '+':
          lexerState.lastToken = Token{ TokenType::Plus };
          break;
        case '-':
          lexerState.lastToken = Token{ TokenType::Minus };
          break;
        case '*':
          lexerState.lastToken = Token{ TokenType::Asterisk };
          break;
        case '/':
          lexerState.lastToken = Token{ TokenType::Slash };
          break;
        case '!':
        {
          if (peekCh == '=')
          {
            lexerState = internal::readChar(lexerState);
            lexerState.lastToken = Token{ TokenType::CompNotEqual };
          }
          else {
            lexerState.lastToken = Token{ TokenType::Bang };
          }
          break;
        }
        case ':':
          lexerState.lastToken = Token{ TokenType::Colon };
          break;
        case '<':
          lexerState.lastToken = Token{ TokenType::LessThan };
          break;
        case '>':
          lexerState.lastToken = Token{ TokenType::GreaterThan };
          break;
        case '[':
          lexerState.lastToken = Token{ TokenType::LBracket };
          break;
        case ']':
          lexerState.lastToken = Token{ TokenType::RBracket };
          break;
        case '{':
          lexerState.lastToken = Token{ TokenType::LBrace };
          break;
        case '}':
          lexerState.lastToken = Token{ TokenType::RBrace };
          break;
        case '"':
          return internal::readString(lexerState);
        case '\0':
          lexerState.lastToken = Token{ TokenType::Eof };
          break;
        default:
          if (isLetter(lexerState.ch)) 
          {
            return internal::readIdentifier(lexerState);
          } 
          else if (isDigit(lexerState.ch))
          {
            return internal::readNumber(lexerState);
          }
          else
          {
            lexerState.lastToken = Token{ TokenType::Illegal, lexerState.input.substr(lexerState.position, 1) };
          }
          break;
      };
      return internal::readChar(lexerState);
    }

  };

  namespace ast 
  {

    struct Identifier
    {
      std::string_view Value{};
      Token token{ TokenType::Ident, "" };

      [[nodiscard]] constexpr bool operator==(const Identifier &) const = default;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const { return token.literal; }

      [[nodiscard]] constexpr std::string String() const { return std::string{Value}; }
    };

    struct IntegerLiteral
    {
      std::int64_t value= 0;
      Token token{ TokenType::Int, "" };

      [[nodiscard]] constexpr bool operator==(const IntegerLiteral &) const = default;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const { return token.literal; }

      [[nodiscard]] constexpr std::string String() const { return std::string{ token.literal }; }
    };

    struct NullLiteral
    {
      Token token{ TokenType::Null};

      [[nodiscard]] constexpr bool operator==(const NullLiteral &) const = default;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const { return token.literal; }

      [[nodiscard]] constexpr std::string String() const { return std::string{ token.literal }; }
    };

    struct BoolLiteral
    {
      bool value{ };
      Token token{ };

      [[nodiscard]] constexpr bool operator==(const BoolLiteral &) const = default;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const { return token.literal; }

      [[nodiscard]] constexpr std::string String() const { return std::string{ token.literal }; }
    };

    struct StringLiteral
    {
      // TODO: should this rather be string instead of string_view?
      std::string_view value{};
      Token token{TokenType::String, ""};
      [[nodiscard]] constexpr bool operator==(const StringLiteral&) const = default;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const {return token.literal;}
      [[nodiscard]] constexpr std::string String() const {return std::string{"\""}+std::string{value}+std::string{"\""};}
    };

    // template<typename TypeToCheck, typename TypeToCheckAgainst>
    // concept TypeIs = std::same_as<std::remove_cvref_t<TypeToCheck>, TypeToCheckAgainst>;

    template<typename TypeToCheck, typename... TypesToCheckAgainst>
    concept TypeIsInList = (std::same_as<std::remove_cvref_t<TypeToCheck>, TypesToCheckAgainst> || ...);

    //template<typename... TypesToCheck, typename... TypesToCheckAgainst>
    //concept TypeListIsInList = (TypeIsInList<TypesToCheck, TypesToCheckAgainst> && ...);

    template<typename subtype>
    concept ParsedGroupSubtype = requires(subtype subtypeInst) { /*subtypeInst.TokenLiteral();*/ subtypeInst.String();};

    template<ParsedGroupSubtype... Ts>
    struct ParsedGroup
    {
      std::variant<Ts...> group;

      constexpr ParsedGroup() = default;

      // TODO: make an explicit constructor from any type in Ts... to this ParsedGroup (into group)
      template<TypeIsInList<Ts...> T>
      constexpr /* implicit */ ParsedGroup(T&& otherType) : group{ std::forward<T>(otherType) } //NOLINT(hicpp-explicit-conversions)
      {
      }

      template<TypeIsInList<Ts...> T>
      constexpr /* implicit */ ParsedGroup(const T& otherType) : group{ otherType } //NOLINT(hicpp-explicit-conversions)
      {
      }
      //template<typename... Ts2>
      //  requires(TypeIsInList<Ts, Ts2> && ...)
      //constexpr ParsedGroup(ParsedGroup<Ts2...> &otherType) : group{otherType.group}
      //{
      //}

      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const
      {
        constexpr auto tokenLiteralVisitor = [](const ParsedGroupSubtype auto &tokenLit) { return tokenLit.TokenLiteral(); };
        return std::visit(tokenLiteralVisitor, group);
      };

      [[nodiscard]] constexpr std::string String() const
      {
        constexpr auto stringVisitor = [](const ParsedGroupSubtype auto &str) { return str.String(); };
        return std::visit(stringVisitor, group);
      };

      [[nodiscard]] constexpr bool operator==(const ParsedGroup<Ts...>& other) const noexcept
      {
        return group == other.group;
      }
    };

    struct PrefixExpression
    {
      std::string_view op;
      // a non-owning pointer to an expression (created and stored as std::unique_ptr's in the parser)
      struct Expression *right = nullptr;
      Token token;

      [[nodiscard]] constexpr bool operator==(const PrefixExpression &other) const;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const { return token.literal; }
      [[nodiscard]] constexpr std::string String() const;
    };

    struct InfixExpression
    {
      // a non-owning pointer to an expression (created and stored as std::unique_ptr's in the parser)
      struct Expression *left = nullptr;
      std::string_view op;
      // a non-owning pointer to an expression (created and stored as std::unique_ptr's in the parser)
      struct Expression *right = nullptr;
      Token token;

      [[nodiscard]] constexpr bool operator==(const InfixExpression &other) const;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const { return token.literal; }
      [[nodiscard]] constexpr std::string String() const;
    };

    struct BlockStatement
    {
      Token token{};
      std::vector<struct Statement> *statements = nullptr;
      [[nodiscard]] constexpr bool operator==(const BlockStatement &other) const;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const { return token.literal; }

      [[nodiscard]] constexpr std::string String() const;
    };

    struct IfExpression
    {
      Token token{TokenType::If};
      struct Expression *Condition = nullptr;
      std::optional<BlockStatement> Consequence;
      std::optional<BlockStatement> Alternative;

      [[nodiscard]] constexpr bool operator==(const IfExpression &other) const;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const { return token.literal; }
      [[nodiscard]] constexpr std::string String() const;
    };

    struct FunctionLiteral
    {
      Token token{ TokenType::Function };
      std::vector<Identifier> *parameters;
      std::optional<BlockStatement> body;
      [[nodiscard]] constexpr bool operator==(const FunctionLiteral &other) const
      {
        return token == other.token && body == other.body
               && (parameters == other.parameters
                   || (parameters != nullptr && other.parameters != nullptr && *parameters == *other.parameters));
      }
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const { return token.literal; }
      [[nodiscard]] constexpr std::string String() const;
    };

    struct CallExpression
    {
      Token token{ TokenType::LParen };
      struct Expression *function;
      std::vector<struct Expression> *arguments;

      [[nodiscard]] constexpr bool operator==(const CallExpression &other) const noexcept;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const { return token.literal; }
      [[nodiscard]] constexpr std::string String() const;
    };

    struct ArrayLiteral
    {
      Token token{TokenType::LBracket, "["};
      std::vector<struct Expression> *elements; 
      [[nodiscard]] constexpr bool operator==(const ArrayLiteral &other) const noexcept = default;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const { return token.literal; }
      [[nodiscard]] constexpr std::string String() const;
    };

    struct HashLiteral
    {
      Token token{TokenType::LBrace, "{"};
      std::vector<std::pair<struct Expression, struct Expression>> *pairs; 
      [[nodiscard]] constexpr bool operator==(const HashLiteral &other) const noexcept = default;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const { return token.literal; }
      [[nodiscard]] constexpr std::string String() const;
    };

    struct IndexExpression
    {
      Token token{TokenType::LBracket, "["};

      struct Expression *left = nullptr;
      struct Expression *index = nullptr;

      [[nodiscard]] constexpr bool operator==(const IndexExpression &other) const noexcept = default;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const { return token.literal; }
      [[nodiscard]] constexpr std::string String() const;
    };

    struct Expression
      : ParsedGroup<Identifier,
          IntegerLiteral,
          NullLiteral,
          BoolLiteral,
          StringLiteral,
          PrefixExpression,
          InfixExpression,
          IfExpression,
          FunctionLiteral,
          CallExpression,
          ArrayLiteral,
          HashLiteral,
          IndexExpression>
    {
      using ParsedGroup::ParsedGroup;
    };

    [[nodiscard]] constexpr std::string PrefixExpression::String() const
    {
      return std::string{ "(" } + std::string{ op } + (right != nullptr ? right->String() : "") + std::string{ ")" };
      // return std::format("({})", (right != nullptr ? right->String() : ""))
    }
    [[nodiscard]] constexpr bool PrefixExpression::operator==(const PrefixExpression &other) const
    {
      return op == other.op
             && ((right == other.right) || (right != nullptr && other.right != nullptr && *right == *other.right));
    }
    [[nodiscard]] constexpr std::string InfixExpression::String() const
    {
      return std::string{ "(" } + (left != nullptr ? left->String() : "") + std::string{ " " } + std::string{ op } + std::string{ " " } + (right != nullptr ? right->String() : "") + std::string{ ")" };
      // return std::format("({} {} {})", (left != nullptr ? left->String() : ""), std::string{op}, (right != nullptr ? right->String() : ""))
    }
    [[nodiscard]] constexpr bool InfixExpression::operator==(const InfixExpression &other) const
    {
      return op == other.op
             && ((left == other.left) || (left != nullptr && other.left != nullptr && *left == *other.left))
             && ((right == other.right) || (right != nullptr && other.right != nullptr && *right == *other.right));
    }

    [[nodiscard]] constexpr std::string IfExpression::String() const
    { 
      std::string out = "if";
      out += Condition != nullptr ? Condition->String() : "";
      out += Consequence ? Consequence->String() : "";
      out += Alternative ? std::string{ "\nelse\n" } + Alternative->String() : "";
      return out;
    }
    [[nodiscard]] constexpr bool IfExpression::operator==(const IfExpression &other) const
    {
      return token == other.token 
        && Consequence == other.Consequence
        && Alternative == other.Alternative
        && (Condition == other.Condition || (Condition != nullptr && other.Condition != nullptr && *Condition == *other.Condition));
    }

    [[nodiscard]] constexpr std::string FunctionLiteral::String() const
    {
      std::string out = std::string{ token.literal };
      out += "(";

      out += parameters == nullptr
        ? "" : (*parameters)
                  | std::views::transform([](const ast::Identifier &ident) -> std::string { return ident.String(); })
                  | std::views::join_with(std::string{ ", " })
                  | std::ranges::to<std::string>();
      out += ")";
      out += body ? body->String() : "{\n}";
      return out;
    }

    [[nodiscard]] constexpr std::string CallExpression::String() const
    {
      std::string out = function != nullptr ? function->String() : "!!!INVALID_FUNCTION!!!";
      out += "(";

      out += arguments == nullptr
               ? ""
               : (*arguments)
                   | std::views::transform([](const ast::Expression &expr) -> std::string { return expr.String(); })
                   | std::views::join_with(std::string{ ", " }) | std::ranges::to<std::string>();
      out += ")";
      return out;
    }

    [[nodiscard]] constexpr bool CallExpression::operator==(const CallExpression &other) const noexcept
    {
      return token == other.token 
        && (function == other.function || (function != nullptr && other.function != nullptr && *function == *other.function))
        && (arguments == other.arguments || (arguments != nullptr && other.arguments != nullptr && *arguments == *other.arguments));
    }

    [[nodiscard]] constexpr std::string ArrayLiteral::String() const
    {
      std::string out;
      out += "[";

      out += elements == nullptr
                ? "" 
                : (*elements)
                  | std::views::transform([](const ast::Expression &expr) -> std::string { return expr.String(); })
                  | std::views::join_with(std::string{ ", " }) | std::ranges::to<std::string>();
      out += "]";
      return out;
    }

    [[nodiscard]] constexpr std::string HashLiteral::String() const
    {
      std::string out;
      out += "{";

      out += pairs == nullptr
                ? "" 
                : (*pairs)
                  | std::views::transform([](const auto& pair) -> std::string { 
                      return pair.first.String() + std::string{":"} + pair.second.String();
                    })
                  | std::views::join_with(std::string{ ", " }) | std::ranges::to<std::string>();
      out += "}";
      return out;
    }

    [[nodiscard]] constexpr std::string IndexExpression::String() const
    {
      std::string out{"("};
      out += left == nullptr ? "" : left->String();
      out += "[";
      out += index == nullptr ? "" : index->String();
      out += "])";
      return out;
    }

    //template<typename V, typename T> struct IsTypeInParsedGroup;

    //template<typename V, typename T0, typename... T> struct IsTypeInParsedGroup<V, ParsedGroup<T0, T...>>
    //{
    //  constexpr static bool value = IsTypeInParsedGroup<V, ParsedGroup<T...>>::value;
    //};

    //template<typename V, typename... T> struct IsTypeInParsedGroup<V, ParsedGroup<V, T...>>
    //{
    //  constexpr static bool value = true;
    //};

    //template<typename V> struct IsTypeInParsedGroup<V, ParsedGroup<>>
    //{
    //  constexpr static bool value = false;
    //};

    //template<typename V, typename... T> inline constexpr bool IsTypeInParsedGroup_v = IsTypeInParsedGroup<V, T...>::value;

    //template<typename TypeToCheck, typename... Ts>
    //concept TypeInParsedGroup = IsTypeInParsedGroup_v<TypeToCheck, Ts...>;

    struct LetStatement
    {
      Identifier Name;
      std::optional<Expression> Value;
      Token token{ TokenType::Let };
      [[nodiscard]] constexpr bool operator==(const LetStatement &) const = default;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const
      { 
        return token.literal;
      }

      [[nodiscard]] constexpr std::string String() const
      { 
        return std::string{TokenLiteral()} + std::string{" "} + Name.String() + std::string{" = "} + (Value ? Value->String() : "") + std::string{";"};
        //return std::format("{} {} = {};", TokenLiteral(), Name.String(), Value ? Value->String() : "");
      }
    };

    struct ReturnStatement 
    {
      std::optional<Expression> ReturnValue;
      Token token{ TokenType::Return };
      [[nodiscard]] constexpr bool operator==(const ReturnStatement &) const = default;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const
      { 
        return token.literal;
      }

      [[nodiscard]] constexpr std::string String() const
      { 

        return std::string{ TokenLiteral() } + std::string{ " " } + (ReturnValue ? ReturnValue->String() : "") + std::string{ ";" };
        //return std::format("{} {};", TokenLiteral(), ReturnValue ? ReturnValue->String() : "");
      }
    };

    struct ExpressionStatement
    {
      std::optional<Expression> expression;
      Token token;

      [[nodiscard]] constexpr bool operator==(const ExpressionStatement &) const = default;

      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const
      { 
        return token.literal;
      }

      [[nodiscard]] constexpr std::string String() const
      { 
        return expression ? /* std::string{expression->TokenLiteral()} +  ":" +*/expression->String() : "";
      }
    };

    struct Statement : ParsedGroup<ExpressionStatement, ReturnStatement, LetStatement, BlockStatement>
    {
      using ParsedGroup::ParsedGroup;
    };

    [[nodiscard]] constexpr std::string BlockStatement::String() const
    {
      std::string out = "{\n";
      for (const auto &stmt : *statements) {
        out += stmt.String();
        out += "\n";
      }
      out += "}";
      return out;
    }
    [[nodiscard]] constexpr bool BlockStatement::operator==(const BlockStatement &other) const
    {
      return token == other.token
             && (statements == other.statements || (statements != nullptr && other.statements != nullptr && *statements == *other.statements));
    }
    //template<typename TypeToCheck>
    //concept TypeInStatement = TypeInParsedGroup<TypeToCheck, Statement>;

    struct Program
    {
      std::vector<Statement> statements;
      [[nodiscard]] inline constexpr std::string_view TokenLiteral() const
      {
        return !statements.empty() ? statements[0].TokenLiteral() : "";
      }

      [[nodiscard]] constexpr std::string String() const
      {
        std::string out;
        for (const auto& stmt : statements) { out += stmt.String(); }
        //for (const auto& stmt : statements) out = std::format("{}{}", out, stmt.String());
        return out;
      }
    };
  }

  struct Parser
  {

    Parser() = default;


    constexpr explicit Parser(LexerState lexerstate) noexcept : lexerState(lexerstate)
    {
      NextToken();
      NextToken();
    }

    [[nodiscard]] constexpr ast::Program ParseProgram(LexerState lexerstate) noexcept
    {
      this->lexerState = lexerstate;
      errors.clear();

      NextToken();
      NextToken();
      return ParseProgram();
    }

    [[nodiscard]] constexpr ast::Program ParseProgram() noexcept
    {
      auto program = ast::Program{};
      while (curToken.type != TokenType::Eof)
      {
        if(auto res = parseStatement(); res.has_value())
        {
          program.statements.push_back(res.value());
        }
        NextToken();
        //else {
        //  break;
        //}
      }
      return program;
    }

    struct Error
    {
      std::size_t position;
      std::string error;
    };
    std::vector<Error> errors;

  private:
    LexerState lexerState;
    Token curToken;
    Token peekToken;

    std::vector<std::unique_ptr<std::vector<std::pair<ast::Expression, ast::Expression>>>> hostedHashMaps;
    std::vector<std::unique_ptr<std::vector<ast::Expression>>> hostedCallExpressionArgumentsLists;
    std::vector<std::unique_ptr<std::vector<ast::Identifier>>> hostedFunctionLiteralParameterLists;
    std::vector<std::unique_ptr<std::vector<ast::Statement>>> hostedBlockStatementLists;
    std::vector<std::unique_ptr<ast::Expression>> hostedExpressions;

    [[nodiscard]] inline constexpr bool curTokenIs(TokenType tok) const { return curToken.type == tok; }

    [[nodiscard]] inline constexpr bool peekTokenIs(TokenType tok) const { return peekToken.type == tok; }

    [[nodiscard]] inline constexpr Precedence curPrecedence() const { return lookup::GetPrecedence(curToken.type); }

    [[nodiscard]] inline constexpr Precedence peekPrecedence() const { return lookup::GetPrecedence(peekToken.type); }

    constexpr void NextToken() noexcept
    {
      curToken = peekToken;
      lexerState = Lexer::NextToken(lexerState);
      peekToken = lexerState.lastToken;
    }

    [[nodiscard]] constexpr ast::Identifier parseIdentifier() const
    {
      return ast::Identifier{ curToken.literal, curToken};
    }

    [[nodiscard]] constexpr ast::StringLiteral parseStringLiteral() const
    {
      return ast::StringLiteral{ curToken.literal, curToken};
    }

    [[nodiscard]] constexpr std::optional<ast::Expression> parseIntegerLiteral()
    {
      std::int64_t parsed = 0;
      auto result = std::from_chars(curToken.literal.data(), curToken.literal.data() + curToken.literal.size(), parsed);//NOLINT(cppcoreguidelines-pro-bounds-pointer-arithmetic)
      if (result.ec == std::errc() && result.ptr == curToken.literal.data() + curToken.literal.size()) //NOLINT(cppcoreguidelines-pro-bounds-pointer-arithmetic)
      {
        return ast::IntegerLiteral{ parsed, curToken };
      } 
      errors.emplace_back(Error{ lexerState.position, std::format("Could not parse {} as integer", curToken.literal) });
      
      return std::nullopt;
    }

    [[nodiscard]] constexpr std::optional<ast::Expression> parseNullLiteral() const
    {
      return ast::NullLiteral{};
    }

    [[nodiscard]] constexpr std::optional<ast::Expression> parseBoolLiteral() const
    {
      return ast::BoolLiteral{ curTokenIs(TokenType::True), curToken };
    }

    [[nodiscard]] constexpr ast::Expression* storeExpression(const ast::Expression &expr)
    {
      std::unique_ptr expr_ptr = std::make_unique<ast::Expression>(expr);
      auto *ptr = expr_ptr.get();
      hostedExpressions.emplace_back(std::move(expr_ptr));
      return ptr;
    }

    [[nodiscard]] constexpr std::vector<ast::Statement>* getNewBlockStatementList()
    {
      std::unique_ptr vec_ptr = std::make_unique<std::vector<ast::Statement>>();
      auto *ptr = vec_ptr.get();
      hostedBlockStatementLists.emplace_back(std::move(vec_ptr));
      return ptr;
    }

    [[nodiscard]] constexpr std::optional<ast::BlockStatement> parseBlockStatement()
    {
      ast::BlockStatement block{ curToken, getNewBlockStatementList()};
      NextToken();

      while (!curTokenIs(TokenType::RBrace) && !curTokenIs(TokenType::Eof))
      {
        if (auto stmt = parseStatement(); stmt.has_value())
        {
          block.statements->push_back(stmt.value());
        }
        NextToken();
      }

      return block.statements->empty() ? std::nullopt : std::optional{ block };
    }

    [[nodiscard]] constexpr std::optional<ast::Expression> parseIfExpression()
    {
      auto ifexpr = ast::IfExpression{ curToken, nullptr, std::nullopt, std::nullopt };

      if (!expectPeek(TokenType::LParen))
      {
        return std::nullopt;
      }
      if (peekTokenIs(TokenType::RParen))
      {
        peekError(TokenType::Null);
        return std::nullopt;
      }

      NextToken();
      if (auto expr = parseExpression(Precedence::LOWEST); expr.has_value())
      {
        ifexpr.Condition = storeExpression(expr.value());
      }
      if (!expectPeek(TokenType::RParen)) { return std::nullopt; }
      if (!expectPeek(TokenType::LBrace)) { return std::nullopt; }
      ifexpr.Consequence = parseBlockStatement();

      if (peekTokenIs(TokenType::Else))
      {
        NextToken();
        if (!expectPeek(TokenType::LBrace)) { return std::nullopt; }
        ifexpr.Alternative = parseBlockStatement();
      }

      return ifexpr;
    }

    [[nodiscard]] constexpr std::vector<ast::Identifier> *getNewFunctionLiteralParameterList()
    {
      std::unique_ptr vec_ptr = std::make_unique<std::vector<ast::Identifier>>();
      auto *ptr = vec_ptr.get();
      hostedFunctionLiteralParameterLists.emplace_back(std::move(vec_ptr));
      return ptr;
    }

    [[nodiscard]] constexpr std::vector<ast::Identifier>* parseFunctionLiteralParameters()
    {
      auto *idents = getNewFunctionLiteralParameterList();

      if (peekTokenIs(TokenType::RParen))
      {
        NextToken();
        return idents;
      }
      NextToken();

      idents->push_back(ast::Identifier{ curToken.literal, curToken });

      while (peekTokenIs(TokenType::Comma))
      {
        NextToken();
        NextToken();
        idents->push_back(ast::Identifier{ curToken.literal, curToken });
      }

      // TODO: pop_back the parameter list from held list
      if (!expectPeek(TokenType::RParen)) { return nullptr; }
      return idents;
    }

    [[nodiscard]] constexpr std::optional<ast::FunctionLiteral> parseFunctionLiteral()
    {
      auto fnexpr = ast::FunctionLiteral{ };

      if (!expectPeek(TokenType::LParen)) { return std::nullopt; }
      fnexpr.parameters = parseFunctionLiteralParameters();

      if (!expectPeek(TokenType::LBrace)) { return std::nullopt; }
      fnexpr.body = parseBlockStatement();

      return fnexpr;
    }

    [[nodiscard]] constexpr std::vector<std::pair<ast::Expression, ast::Expression>> *getNewHashMap()
    {
      std::unique_ptr map_ptr = std::make_unique<std::vector<std::pair<ast::Expression, ast::Expression>>>();
      auto *ptr = map_ptr.get();
      hostedHashMaps.emplace_back(std::move(map_ptr));
      return ptr;
    }

    [[nodiscard]] constexpr std::vector<ast::Expression> *getNewExpressionList()
    {
      std::unique_ptr vec_ptr = std::make_unique<std::vector<ast::Expression>>();
      auto *ptr = vec_ptr.get();
      hostedCallExpressionArgumentsLists.emplace_back(std::move(vec_ptr));
      return ptr;
    }

    [[nodiscard]] constexpr std::vector<ast::Expression> *parseExpressionList(TokenType EndToken)
    {
      auto* args = getNewExpressionList();
      if (peekTokenIs(EndToken))
      {
        NextToken();
        return args;
      }

      NextToken();

      if (auto expr = parseExpression(Precedence::LOWEST); expr.has_value())
      {
        args->push_back(expr.value());
      }

      while (peekTokenIs(TokenType::Comma))
      {
        NextToken();
        NextToken();
        if (auto expr = parseExpression(Precedence::LOWEST); expr.has_value())
        {
          args->push_back(expr.value());
        }
      }

      if (!expectPeek(EndToken))
      {
        return nullptr;
      }

      return args;
    }

    [[nodiscard]] constexpr std::vector<ast::Expression> *parseCallExpressionArguments()
    {
      return parseExpressionList(TokenType::RParen);
      
    }

    [[nodiscard]] constexpr std::optional<ast::CallExpression> parseCallExpression(ast::Expression* function)
    {
      auto cexpr = ast::CallExpression{ curToken, function, parseCallExpressionArguments() };
      return cexpr;
    }

    [[nodiscard]] constexpr std::optional<ast::Expression> parseGroupedExpression()
    {
      NextToken();

      auto expr = parseExpression(Precedence::LOWEST);
      if (!expectPeek(TokenType::RParen))
      {
        return std::nullopt;
      }
      return expr;
    }

    [[nodiscard]] constexpr std::optional<ast::Expression> parseIndexExpression(ast::Expression* left) 
    {
      auto idxExpr = ast::IndexExpression{curToken, left, nullptr};
      NextToken();

      if (auto expr = parseExpression(Precedence::LOWEST); expr.has_value())
      {
        idxExpr.index = storeExpression(expr.value());
      }

      if(!expectPeek(TokenType::RBracket))
      {
        return std::nullopt;
      }

      return idxExpr;
    }

    [[nodiscard]] constexpr std::optional<ast::Expression> parseInfixExpression(ast::Expression* left)
    {
      if(curToken.type == TokenType::LParen)
      {
        return parseCallExpression(left);
      }
      else if(curToken.type == TokenType::LBracket)
      {
        return parseIndexExpression(left);
      }
      else
      {
        auto infix = ast::InfixExpression{left, curToken.literal, nullptr, curToken};
        auto curPrec = curPrecedence();
        NextToken();
        if (auto expr = parseExpression(curPrec); expr.has_value())
        {
          infix.right = storeExpression(expr.value());
        }
        else
        {
          // TODO: warn or something that our infix expression has no RHS????
        }
        return infix;
      }
    }

    [[nodiscard]] constexpr std::optional<ast::Expression> parsePrefixExpression()
    {
      auto prefix = ast::PrefixExpression{ curToken.literal, nullptr, curToken };

      NextToken();
 
      if (auto expr = parseExpression(Precedence::PREFIX); expr.has_value())
      {
        prefix.right = storeExpression(expr.value());
        return prefix;
      }
      errors.emplace_back(lexerState.position, std::format("Unable to Parse Prefix for {}", lookup::TokenString(curToken.type)));
      return std::nullopt;
    }

    [[nodiscard]] constexpr std::optional<ast::Expression> parseArrayLiteral()
    {
      return ast::ArrayLiteral{curToken, parseExpressionList(TokenType::RBracket)};
    }

    [[nodiscard]] constexpr std::optional<ast::Expression> parseHashLiteral()
    {
      auto lit = ast::HashLiteral{curToken, getNewHashMap()};
      while (!peekTokenIs(TokenType::RBrace))
      {
        NextToken();
        auto key = parseExpression(Precedence::LOWEST);
        if(!expectPeek(TokenType::Colon))
        {
          return std::nullopt;
        }
        if(!key.has_value())
        {
          errors.emplace_back(lexerState.position, std::string{"unable to parse map key"});
          return std::nullopt;
        }

        NextToken();
        auto value = parseExpression(Precedence::LOWEST);
        if(value.has_value())
        {
          lit.pairs->emplace_back(key.value(),value.value());
        }
        else 
        {
          errors.emplace_back(lexerState.position, std::string{"unable to parse map value"});
          return std::nullopt;
        }
        if(!peekTokenIs(TokenType::RBrace) && !expectPeek(TokenType::Comma))
        {
          return std::nullopt;
        }
      }
      NextToken();

      return lit;
    }

    [[nodiscard]] constexpr std::optional<ast::Expression> parseExpression(Precedence precedence)
    {
      std::optional<ast::Expression> leftExpr_opt = std::nullopt;
      // NOTE: these are the prefix functions:
      switch (curToken.type)
      {
        case TokenType::Function:
          leftExpr_opt = parseFunctionLiteral();
          break;
        case TokenType::Ident:
          leftExpr_opt = parseIdentifier();
          break;
        case TokenType::If:
          leftExpr_opt = parseIfExpression();
          break;
        case TokenType::String:
          leftExpr_opt = parseStringLiteral();
          break;
        case TokenType::Int:
          leftExpr_opt = parseIntegerLiteral();
          break;
        case TokenType::True:
          [[fallthrough]];
        case TokenType::False:
          leftExpr_opt = parseBoolLiteral();
          break;
        case TokenType::Null:
          leftExpr_opt = parseNullLiteral();
          break;
        case TokenType::Plus:
          [[fallthrough]];
        case TokenType::Bang:
          [[fallthrough]];
        case TokenType::Minus:
          leftExpr_opt = parsePrefixExpression();
          break;
        case TokenType::LParen:
          leftExpr_opt = parseGroupedExpression();
          break;
        case TokenType::LBracket:
          leftExpr_opt = parseArrayLiteral();
          break;
        case TokenType::LBrace:
          leftExpr_opt = parseHashLiteral();
          break;
        default:
          break;
      }
      if (!leftExpr_opt.has_value())
      {
        errors.emplace_back(Error{ lexerState.position, std::format("No Prefix function found for {}", curToken.literal) });
        return std::nullopt;
      }

      while (!peekTokenIs(TokenType::Semicolon) && precedence < peekPrecedence())
      {
        // TODO: why was this here?
        // if (peekPrecedence() <= Precedence::LOWEST)
        // {
        //   return leftExpr_opt;
        // }

        NextToken();
        auto leftExpr_p = leftExpr_opt.value();
        leftExpr_opt = parseInfixExpression(storeExpression(leftExpr_p));
        if (!leftExpr_opt.has_value())
        {
          errors.emplace_back(Error{ lexerState.position, std::format("No Infix function found for {}", curToken.literal) });
          return std::nullopt;
        }
      }
      return leftExpr_opt;
    };

    static constexpr auto toStatement = [](const auto &subtype) { return std::optional<ast::Statement>{ subtype }; };
    [[nodiscard]] constexpr std::optional<ast::LetStatement> parseLetStatement()
    {
      ast::LetStatement stmt{ { curToken.literal, curToken }, std::nullopt };

      if (auto wasExpectedToken = expectPeek(TokenType::Ident); !wasExpectedToken) { return std::nullopt; } 

      stmt.Name = parseIdentifier();

      if (auto wasExpectedToken = expectPeek( TokenType::Assign); !wasExpectedToken) { return std::nullopt; }

      NextToken();
      stmt.Value = parseExpression(Precedence::LOWEST);

      if (peekTokenIs(TokenType::Semicolon))
      {
        NextToken();
      }

      return stmt;
    };

    [[nodiscard]] constexpr ast::ReturnStatement parseReturnStatement()
    {
      ast::ReturnStatement stmt;
      stmt.token = curToken;

      NextToken();

      stmt.ReturnValue = parseExpression(Precedence::LOWEST);

      if (peekTokenIs(TokenType::Semicolon))
      {
        NextToken();
      }

      return stmt;
    };

    [[nodiscard]] constexpr ast::ExpressionStatement parseExpressionStatement()
    {
      ast::ExpressionStatement stmt{ parseExpression(Precedence::LOWEST), curToken };

      if (peekTokenIs(TokenType::Semicolon)) { NextToken(); }

      return stmt;
    };

    [[nodiscard]] constexpr std::optional<ast::Statement> parseStatement()
    { 
      switch (curToken.type) 
      {
        case TokenType::Let:
        {
        return parseLetStatement();
        //.and_then(toStatement);
          break;
        }
        case TokenType::Return:
        {
          return parseReturnStatement();
          break;
        }
        default:
          return parseExpressionStatement();
          break;
      }
      return std::nullopt;
    }

    [[nodiscard]] constexpr bool expectPeek(TokenType token)
    { 
      if (peekTokenIs(token))
      {
        NextToken();
        return  true;
      }
      peekError(token);
      return false;
    }

    constexpr void peekError(TokenType token)
    { 
      errors.emplace_back(Error{ lexerState.readPosition,
        std::format("Expected {} and got {}", lookup::TokenString(token), lookup::TokenString(peekToken.type)) });
    }

  };

  struct Environment;
  namespace Object
  {

    struct NullObject
    {
      constexpr static std::string_view TypeName = "null";
      constexpr bool operator==([[maybe_unused]] const NullObject &other) const { return true; }
      [[nodiscard]] constexpr static HashKey Hash() { return HashKey{TypeName, 0}; }
      [[nodiscard]] constexpr static std::string String() { return std::string{ TypeName }; }
    };

    struct IntegerObject
    {
      std::int64_t Value{ 0 };
      //std::string_view SourceLit;
      constexpr static std::string_view TypeName = "int";
      constexpr bool operator==(const IntegerObject &) const = default;
      [[nodiscard]] constexpr HashKey Hash() const { return HashKey{TypeName, std::hash<std::int64_t>{}(Value)}; }
      [[nodiscard]] constexpr std::string String() const
      {
        constexpr unsigned long long buf_size = 100;
        std::array<char, buf_size> buf{};
        //todo: std::to_string is not constexpr, so we just do this for now:
        auto [ptr, ec] = std::to_chars(buf.data(), buf.data() + buf_size, Value);//NOLINT(cppcoreguidelines-pro-bounds-pointer-arithmetic)
        return ec == std::errc() ? std::string{buf.data(),static_cast<size_t>(std::distance(buf.data(),ptr))} : "failed to parse int to string";
        //return std::to_string(Value);
      }
    };

    struct BoolObject
    {
      bool Value{};
      constexpr static std::string_view TypeName = "bool";
      constexpr bool operator==(const BoolObject&) const = default;
      [[nodiscard]] constexpr HashKey Hash() const { return HashKey{TypeName, static_cast<std::uint64_t>(Value)}; }
      [[nodiscard]] constexpr std::string String() const { return Value ? "true" : "false"; }
    };

    struct StringObject
    {
      std::string Value{};
      constexpr static std::string_view TypeName = "string";
      [[nodiscard]] constexpr HashKey Hash() const { return HashKey{TypeName, std::hash<std::string>{}(Value)}; }
      constexpr bool operator==(const StringObject&) const = default;
      [[nodiscard]] constexpr std::string String() const { return Value; }
    };

    struct ReturnObject
    {
      struct Object *Value;
      constexpr static std::string_view TypeName = "<RETURN_OBJECT>";
      [[nodiscard]] constexpr HashKey Hash() const;
      constexpr bool operator==(const ReturnObject &other) const {
        return Value == other.Value;
      };
      [[nodiscard]] constexpr std::string String() const;
    };

    struct ErrorObject
    {
      // TODO: ideally we could make this a string_view (but then there's the question of how we add non-static info like identifier names and values)
      std::string Error;
      constexpr static std::string_view TypeName = "<ERROR_OBJECT>";
      constexpr bool operator==(const ErrorObject &other) const { return Error == other.Error; };
      [[nodiscard]] constexpr HashKey Hash() const { return HashKey{TypeName, std::hash<std::string_view>{}(Error)}; }
      [[nodiscard]] constexpr std::string String() const
      {
        return std::string { Error };
      }
    };

    struct Object;
    // TODO: this should probably be a std::span<Object> instead of std::vector<Object>
    using BuiltinFunctionType = Object(std::vector<Object>);
    struct FunctionObject
    {
      std::vector<ast::Identifier>* parameters = nullptr;
      std::int8_t numParms = 0;
      std::optional<ast::BlockStatement> body;
      std::optional<std::function<BuiltinFunctionType>*> builtin;
      mpp::Environment *Env = nullptr;
      constexpr static std::string_view TypeName = "<fn>";
      [[nodiscard]] constexpr HashKey Hash() const { return HashKey{TypeName, 0}; }
      constexpr bool operator==(const FunctionObject &other) const
      {
        // constexpr auto builtinToPointer = [](decltype(builtin.value()) fn){return fn.target<BuiltinFunctionType>();};
        return *parameters == *other.parameters && body == other.body 
          && builtin == other.builtin;
          // && ( builtin.transform(builtinToPointer) == other.builtin.transform(builtinToPointer));
          //&&*Env == *other.Env;
      };
      [[nodiscard]] constexpr std::string String() const
      {
        std::string out = "fn";
        out += "(";

        if (parameters != nullptr)
        {
          out += (*parameters)
                 | std::views::transform([](const ast::Identifier &ident) -> std::string { return ident.String(); })
                 | std::views::join_with(std::string{ ", " }) | std::ranges::to<std::string>();
        }
        out += ")";
        out += body ? body->String() : builtin ? "{\n--builtin--\n}" : "{\n}";
        return out;
      }
    };

    struct ArrayObject
    {
      std::vector<Object> Value;

      constexpr static std::string_view TypeName = "array";
      [[nodiscard]] constexpr HashKey Hash() const { return HashKey{TypeName, 0}; }
      constexpr bool operator==(const ArrayObject&) const = default;
      [[nodiscard]] constexpr std::string String() const; 
    };

    struct HashObject
    {

      std::unordered_map<HashKey, std::pair<Object, Object>> Value;

      constexpr static std::string_view TypeName = "hash";
      [[nodiscard]] constexpr HashKey Hash() const { return HashKey{TypeName, 0}; }
      [[nodiscard]] constexpr bool operator==(const HashObject& other) const;// = default;
      [[nodiscard]] constexpr std::string String() const; 
    };

    struct Object : ast::ParsedGroup<NullObject, IntegerObject, BoolObject, StringObject, ReturnObject, ErrorObject, FunctionObject, ArrayObject, HashObject>
    {
      using ParsedGroup::ParsedGroup;
      using ParsedGroup::String;
      [[nodiscard]] constexpr HashKey Hash() const {return std::visit([](const auto& x){return x.Hash(); },group);}
      [[nodiscard]] constexpr std::string_view getTypeString() const {return std::visit([](const auto& x){return x.TypeName; },group);}
    };

    [[nodiscard]] constexpr HashKey ReturnObject::Hash() const { return Value->Hash(); }
    constexpr bool HashObject::operator==([[maybe_unused]] const HashObject& other) const
    {
      return Value == other.Value;
      // return String() == other.String();
    }

    [[nodiscard]] constexpr std::string ReturnObject::String() const { return Value->String(); }
    [[nodiscard]] constexpr std::string ArrayObject::String() const 
    { 
      std::string str{"["};
      str += Value
              | std::views::transform([](const Object &obj) -> std::string { return obj.String(); })
              | std::views::join_with(std::string{ ", " }) | std::ranges::to<std::string>();
      str += "]";
      return str;
    }
    [[nodiscard]] constexpr std::string HashObject::String() const 
    { 
      std::string str{"["};
      str += Value
              | std::views::transform([](const auto &obj) -> std::string { return obj.second.first.String() + std::string{":"} + obj.second.second.String(); })
              | std::views::join_with(std::string{ ", " }) | std::ranges::to<std::string>();
      str += "]";
      return str;
    }

    template<typename T>
    concept NotReturnOrNullObject = !std::is_same_v<T, ReturnObject> && !std::is_same_v<T, NullObject>;
  }

  template<class... Ts> struct overload : Ts... { using Ts::operator()...; };
  template<class... Ts> overload(Ts...) -> overload<Ts...>;

  struct Environment
  {
    Environment *parent = nullptr;
    std::vector<std::pair<std::string, Object::Object>> vars;

    constexpr explicit Environment(Environment *parent) : parent(parent) {}
    constexpr Environment() = default;
    constexpr Environment(const Environment &) = default;
    constexpr Environment(Environment &&) = default;
    constexpr Environment &operator=(const Environment &) = default;
    constexpr Environment &operator=(Environment &&) = delete;
    constexpr ~Environment() = default;

    [[nodiscard]] std::optional<Object::Object> Get(std::string_view name) const noexcept
    {
      auto* env = this;
      while(env != nullptr)
      {
        if (auto iter = std::ranges::find(env->vars, std::string{ name }, &std::pair<std::string, Object::Object>::first); iter != env->vars.end()) 
        {
          return { iter->second };
        }
        env = env->parent;
      }
      return std::nullopt;
    }

    constexpr void Add(std::string_view name,const Object::Object& value)
    {
      //vars.emplace(std::string{ name }, value);
      //vars[std::string{ name }] = value;
      if (auto iter = std::ranges::find(vars, std::string{ name }, &std::pair<std::string, Object::Object>::first); iter != vars.end()) 
      {
        iter->second = value;
      }
      vars.emplace_back(name, value);
    }
  };

  struct Evaluator
  {
    std::vector<std::unique_ptr<Object::Object>> heldObjects;
    std::vector<Environment*> EnvStack;
    std::vector<std::unique_ptr<Environment>> HostedEnvironments;
    std::vector<std::function<Object::BuiltinFunctionType>> BuiltinFunctions;

    constexpr Evaluator() 
    { 
      pushEnv(getNewEnv());

      registerBuiltin("null", Object::NullObject{});

      constexpr int nums = 50;
      BuiltinFunctions.reserve(nums);
      registerBuiltin("len", 1, [](std::vector<Object::Object> parms)->Object::Object{
        // NOTE: we don't have to verify the number of parameters as that's done at the callsite already
        const auto &parm = parms[0];
        constexpr auto len = overload {
          [](const Object::StringObject &str) { return Object::Object{Object::IntegerObject{static_cast<std::int64_t>(str.Value.size())}}; },
          [](const Object::ArrayObject &arr) { return Object::Object{Object::IntegerObject{static_cast<std::int64_t>(arr.Value.size())}}; },
          [](const auto &def) { return Object::Object{Object::ErrorObject{"len() called with incorrect Type: " + std::string{def.TypeName}}}; }
        };
        return std::visit(len, parm.group);
      });
      registerBuiltin("first", 1, [](std::vector<Object::Object> parms)->Object::Object{
        const auto &parm = parms[0];
        constexpr auto first = overload {
          [](const Object::StringObject &str) { return !str.Value.empty() ? Object::Object{Object::StringObject{std::string{str.Value.front()}}} : Object::Object{Object::NullObject{}}; },
          [](const Object::ArrayObject &arr) { return arr.Value.empty() ? Object::NullObject{} : arr.Value.front(); },
          [](const auto &def) { return Object::Object{Object::ErrorObject{"first() called with incorrect Type: " + std::string{def.TypeName}}}; }
        };
        return std::visit(first, parm.group);
      });
      registerBuiltin("last", 1, [](std::vector<Object::Object> parms)->Object::Object{
        const auto &parm = parms[0];
        constexpr auto last = overload {
          [](const Object::StringObject &str) { return !str.Value.empty() ? Object::Object{Object::StringObject{std::string{str.Value.back()}}} : Object::Object{Object::NullObject{}}; },
          [](const Object::ArrayObject &arr) { return arr.Value.empty() ? Object::NullObject{} : arr.Value.back(); },
          [](const auto &def) { return Object::Object{Object::ErrorObject{"first() called with incorrect Type: " + std::string{def.TypeName}}}; }
        };
        return std::visit(last, parm.group);
      });
      registerBuiltin("rest", 1, [](std::vector<Object::Object> parms)->Object::Object{
        const auto &parm = parms[0];
        constexpr auto rest = overload {
          [](const Object::StringObject &str) { return str.Value.length() > 1 ? Object::Object{Object::StringObject{str.Value.substr(1, str.Value.length()-1)}} : Object::Object{Object::NullObject{}}; },
          [](const Object::ArrayObject &arr) { return arr.Value.empty() ? Object::Object{Object::NullObject{}} : Object::Object{Object::ArrayObject{arr.Value | std::views::drop(1) | std::ranges::to<std::vector>()}}; },
          [](const auto &def) { return Object::Object{Object::ErrorObject{"rest() called with incorrect Type: " + std::string{def.TypeName}}}; }
        };
        return std::visit(rest, parm.group);
      });
      registerBuiltin("push", 2, [](std::vector<Object::Object> parms)->Object::Object{
        const auto &parm_arr_str = parms[0];
        const auto &parm_obj = parms[1];
        constexpr auto push = overload {
          [](const Object::StringObject &str, const Object::StringObject& obj) { return Object::Object{Object::StringObject{std::string{str.Value + obj.Value}}}; },
          [](const Object::ArrayObject &arr, const auto& obj) { auto arr2 = arr; arr2.Value.push_back(obj); return Object::Object{arr2}; },
          [](const auto &def, [[maybe_unused]] const auto& obj) { return Object::Object{Object::ErrorObject{"push() called with incorrect Type: " + std::string{def.TypeName}}}; }
        };
        return std::visit(push, parm_arr_str.group, parm_obj.group);
      });
      registerBuiltin("puts", -1, [](const std::vector<Object::Object> &parms)->Object::Object{
        for(const auto& obj : parms)
        {
          std::cout<<obj.String();
        }
        std::cout<<"\n";
        return Object::NullObject{};
      });
    }
    constexpr void pushEnv(Environment* env)
    {
      EnvStack.emplace_back(env);
    }
    constexpr void popEnv()
    {
      EnvStack.pop_back();
    }
    constexpr Environment* getEnv()
    {
      if (!EnvStack.empty())
      {
        return EnvStack.back();
      }
      auto* env = getNewEnv();
      pushEnv(env);
      return env;
    }

    struct EnvPushPopper
    {
      Evaluator *eval;
      constexpr EnvPushPopper(const EnvPushPopper &) = default;
      constexpr EnvPushPopper(EnvPushPopper &&) = delete;
      constexpr EnvPushPopper &operator=(const EnvPushPopper &) = default;
      constexpr EnvPushPopper &operator=(EnvPushPopper &&) = delete;
      constexpr EnvPushPopper(Evaluator &eval, Environment *env) : eval(&eval) 
      {
        eval.pushEnv(env); 
      }
      constexpr explicit EnvPushPopper(Evaluator& eval) : eval(&eval)
      {
        eval.pushEnv(eval.getNewEnv(eval.getEnv()));
      }
      constexpr ~EnvPushPopper()
      {
        eval->popEnv();
      }
    };

    [[nodiscard]] std::optional<Object::Object> getVar(std::string_view name) const noexcept
    {
      if (!EnvStack.empty()) 
      {
        return EnvStack.back()->Get(name);
      }
      return std::nullopt;
    }

    void constexpr addVar(std::string_view name,const Object::Object& value) 
    {
      EnvStack.back()->Add(name, value);
    }

    [[nodiscard]] constexpr Object::Object *getNewObject(const Object::Object &obj)
    {
      auto unique_ptr = std::make_unique<Object::Object>(obj);
      auto *ptr = unique_ptr.get();
      heldObjects.push_back(std::move(unique_ptr));
      return ptr;
    }

    [[nodiscard]] constexpr Environment* getNewEnv(Environment* parent = nullptr)
    {
      auto unique_ptr = std::make_unique<Environment>(Environment{ parent });
      auto *ptr = unique_ptr.get();
      HostedEnvironments.push_back(std::move(unique_ptr));
      return ptr;
    }

    [[nodiscard]] static bool IsTruthy(Object::Object obj) noexcept
    {
      constexpr auto truthyEval= overload { 
        [](const Object::IntegerObject &def) { return static_cast<bool>(def.Value); },
        [](const Object::ReturnObject &def) { return Evaluator{}.IsTruthy(*def.Value); },
        [](const Object::BoolObject &def) { return def.Value; },
        []([[maybe_unused]] const auto &def) { return false; },
      };
      return std::visit(truthyEval, obj.group);
    }
    // TODO: again, this can all be done in one place instead of yet another layer of separation...
    
    [[nodiscard]] Object::Object operator()(const ast::Identifier &ident)
    {
      if (auto var = getVar(ident.Value); var.has_value())
      {
        return var.value();
      }
      return Object::ErrorObject{ std::string{ "Identifier Not Found: " } + std::string{ ident.Value } };
    }

    [[nodiscard]] Object::Object operator()(const ast::BoolLiteral &boolLit)
    {
      return Object::BoolObject{ boolLit.value };
    }

    [[nodiscard]] Object::Object operator()([[maybe_unused]] const ast::NullLiteral &nullLit)
    {
      return Object::NullObject{};
    }

    [[nodiscard]] Object::Object operator()(const ast::IntegerLiteral &intLit)
    {
      return Object::IntegerObject{ intLit.value };
    }

    [[nodiscard]] Object::Object operator()(const ast::StringLiteral &stringLiteral)
    {
      return Object::StringObject{ std::string{stringLiteral.value} };
    }

    template <typename Derived>
    struct recursiveObjectVisitor
    {
      Object::Object operator()(Object::ReturnObject lhs, Object::ReturnObject rhs) { return std::visit(*static_cast<Derived*>(this), lhs.Value->group, rhs.Value->group); };
      Object::Object operator()(auto lhs, Object::ReturnObject rhs) { return std::visit(*static_cast<Derived*>(this), Object::Object{lhs}.group, rhs.Value->group); };
      Object::Object operator()(Object::ReturnObject lhs, auto rhs ) { return std::visit(*static_cast<Derived*>(this),lhs.Value->group, Object::Object{rhs}.group); };
    };

    // TODO: we can try and store an std::array<std::string_view, ??> in ErrorObject, or maybe the range over that joined with spaces?
    template <typename Derived>
    struct defaultErrorVisitor
    {
      constexpr static std::string_view TypeMismatchStr = "Type Mismatch: ";
      constexpr static std::string_view UnknownOperatorStr = "Unknown Operator: ";
      Object::Object operator()(const auto &lhs, const auto &rhs)
      {
        constexpr bool isTypeMismatch = !std::is_same_v<decltype(lhs), decltype(rhs)>;
        return Object::ErrorObject{ (isTypeMismatch ? std::string{ TypeMismatchStr }
                                                    : std::string{ UnknownOperatorStr })
                                    + std::string{ lhs.TypeName } + "(" + lhs.String() + ")" + std::string{ Derived::Operator}
                                    + std::string{ rhs.TypeName } + "(" + rhs.String() + ")" };
      };
      Object::Object operator()(const auto &lhs)
      {
        return Object::ErrorObject{std::string{UnknownOperatorStr} + std::string{ Derived::Operator } + std::string{ lhs.TypeName }+ "(" + lhs.String() + ")"  };
      };
      // bubble errors up:
      //constexpr Object::Object operator()(const Object::ErrorObject& lhs, [[maybe_unused]] const auto& rhs) { return lhs; }
      //constexpr Object::Object operator()([[maybe_unused]] const auto& lhs,const Object::ErrorObject& rhs) { return rhs; }
      Object::Object operator()(const Object::ErrorObject& lhs) { return lhs; }
    };

    struct addInfixEval
      : recursiveObjectVisitor<addInfixEval>
      , defaultErrorVisitor<addInfixEval>
    {
      constexpr static std::string_view Operator = "+";
      using recursiveObjectVisitor::operator();
      using defaultErrorVisitor::operator();
      Object::Object operator()(const Object::StringObject &lhs, const Object::StringObject &rhs) { return Object::StringObject{ lhs.Value + rhs.Value}; };
      Object::Object operator()(const Object::IntegerObject &lhs, const Object::IntegerObject &rhs) { return Object::IntegerObject{ lhs.Value + rhs.Value}; };
    };

    struct subInfixEval : recursiveObjectVisitor<subInfixEval>, defaultErrorVisitor<subInfixEval>
    {
      constexpr static std::string_view Operator = "-";
      using recursiveObjectVisitor::operator();
      using defaultErrorVisitor::operator();
      Object::Object operator()(const Object::IntegerObject &lhs, const Object::IntegerObject &rhs)
      {
        return Object::IntegerObject{ lhs.Value - rhs.Value };
      };
    };

    struct mulInfixEval : recursiveObjectVisitor<mulInfixEval>
      , defaultErrorVisitor<mulInfixEval>
    {
      constexpr static std::string_view Operator = "*";
      using recursiveObjectVisitor::operator();
      using defaultErrorVisitor::operator();
      Object::Object operator()(const Object::IntegerObject &lhs, const Object::IntegerObject &rhs)
      {
        return Object::IntegerObject{ lhs.Value * rhs.Value };
      };
    };

    struct divInfixEval : recursiveObjectVisitor<divInfixEval>
      , defaultErrorVisitor<divInfixEval>
    {
      constexpr static std::string_view Operator = "/";
      using defaultErrorVisitor::operator();
      using recursiveObjectVisitor::operator();
      Object::Object operator()(const Object::IntegerObject &lhs, const Object::IntegerObject &rhs)
      {
        return Object::IntegerObject{ lhs.Value / rhs.Value };
      };
    };

    struct eqInfixEval : recursiveObjectVisitor<eqInfixEval>
      , defaultErrorVisitor<eqInfixEval>
    {
      constexpr static std::string_view Operator = "==";
      using recursiveObjectVisitor::operator();
      using defaultErrorVisitor::operator();
      Object::Object operator()(const Object::StringObject &lhs, const Object::StringObject &rhs) { return Object::BoolObject{lhs.Value == rhs.Value }; };
      Object::Object operator()(const Object::IntegerObject &lhs, const Object::IntegerObject &rhs) { return Object::BoolObject{lhs.Value == rhs.Value }; };
      Object::Object operator()(const Object::BoolObject &lhs, const Object::BoolObject &rhs) { return Object::BoolObject{lhs.Value == rhs.Value }; };
      Object::Object operator()([[maybe_unused]] const Object::NullObject &lhs, [[maybe_unused]] const Object::NullObject &rhs) { return Object::BoolObject{true }; };
      Object::Object operator()([[maybe_unused]] const Object::NullObject &lhs, [[maybe_unused]] const Object::NotReturnOrNullObject auto &rhs) { return Object::BoolObject{false }; };
      Object::Object operator()([[maybe_unused]] const Object::NotReturnOrNullObject auto &lhs, [[maybe_unused]] const Object::NullObject &rhs) { return Object::BoolObject{false }; };
    };
    struct neqInfixEval : recursiveObjectVisitor<eqInfixEval>
      , defaultErrorVisitor<neqInfixEval>
    {
      constexpr static std::string_view Operator = "!=";
      using recursiveObjectVisitor::operator();
      using defaultErrorVisitor::operator();
      Object::Object operator()(const Object::StringObject &lhs, const Object::StringObject &rhs) { return Object::BoolObject{lhs.Value != rhs.Value}; };
      Object::Object operator()(const Object::IntegerObject &lhs, const Object::IntegerObject &rhs) { return Object::BoolObject{lhs.Value != rhs.Value}; };
      Object::Object operator()(const Object::BoolObject &lhs, const Object::BoolObject &rhs) { return Object::BoolObject{lhs.Value != rhs.Value}; };
      Object::Object operator()([[maybe_unused]] const Object::NullObject &lhs, [[maybe_unused]] const Object::NullObject &rhs) { return Object::BoolObject{false}; };
      Object::Object operator()([[maybe_unused]] const Object::NullObject &lhs, [[maybe_unused]] const Object::NotReturnOrNullObject auto &rhs) { return Object::BoolObject{true }; };
      Object::Object operator()([[maybe_unused]] const Object::NotReturnOrNullObject auto &lhs, [[maybe_unused]] const Object::NullObject &rhs) { return Object::BoolObject{true }; };
    };
    struct greatInfixEval : recursiveObjectVisitor<greatInfixEval>
      , defaultErrorVisitor<greatInfixEval>
    {
      constexpr static std::string_view Operator = ">";
      using recursiveObjectVisitor::operator();
      using defaultErrorVisitor::operator();
      Object::Object operator()(const Object::StringObject &lhs, const Object::StringObject &rhs) { return Object::BoolObject{lhs.Value > rhs.Value}; };
      Object::Object operator()(const Object::IntegerObject &lhs, const Object::IntegerObject &rhs) { return Object::BoolObject{lhs.Value > rhs.Value}; };
      Object::Object operator()(const Object::BoolObject &lhs, const Object::BoolObject &rhs) { return Object::BoolObject{static_cast<int>(lhs.Value) > static_cast<int>(rhs.Value)}; };
      Object::Object operator()([[maybe_unused]] const Object::NullObject &lhs, [[maybe_unused]] const Object::NullObject &rhs) { return Object::BoolObject{false}; };
      Object::Object operator()([[maybe_unused]] const Object::NullObject &lhs, [[maybe_unused]] const Object::NotReturnOrNullObject auto &rhs) { return Object::BoolObject{false}; };
      Object::Object operator()([[maybe_unused]] const Object::NotReturnOrNullObject auto &lhs, [[maybe_unused]] const Object::NullObject &rhs) { return Object::BoolObject{true}; };
    };
    struct lessInfixEval : recursiveObjectVisitor<greatInfixEval>
      , defaultErrorVisitor<lessInfixEval>
    {
      constexpr static std::string_view Operator = "<";
      using recursiveObjectVisitor::operator();
      using defaultErrorVisitor::operator();
      Object::Object operator()(const Object::StringObject &lhs, const Object::StringObject &rhs) { return Object::BoolObject{lhs.Value < rhs.Value}; };
      Object::Object operator()(const Object::IntegerObject &lhs, const Object::IntegerObject &rhs) { return Object::BoolObject{lhs.Value < rhs.Value}; };
      Object::Object operator()(const Object::BoolObject &lhs, const Object::BoolObject &rhs) { return Object::BoolObject{static_cast<int>(lhs.Value) < static_cast<int>(rhs.Value)}; };
      Object::Object operator()([[maybe_unused]] const Object::NullObject &lhs, [[maybe_unused]] const Object::NullObject &rhs) { return Object::BoolObject{false}; };
      Object::Object operator()([[maybe_unused]] const Object::NullObject &lhs, [[maybe_unused]] const Object::NotReturnOrNullObject auto &rhs) { return Object::BoolObject{true}; };
      Object::Object operator()([[maybe_unused]] const Object::NotReturnOrNullObject auto &lhs, [[maybe_unused]] const Object::NullObject &rhs) { return Object::BoolObject{false}; };
    };

    [[nodiscard]] Object::Object operator()(const ast::InfixExpression &infix)
    {
      const auto lhs = (*this)(*infix.left);
      const auto rhs = (*this)(*infix.right);

      if (infix.op == "+")
      {
        return std::visit(addInfixEval{}, lhs.group, rhs.group);
      }
      if (infix.op == "-")
      {
        return std::visit(subInfixEval{}, lhs.group, rhs.group);
      }
      if (infix.op == "*")
      {
        return std::visit(mulInfixEval{}, lhs.group, rhs.group);
      }
      if (infix.op == "/")
      {
        return std::visit(divInfixEval{}, lhs.group, rhs.group);
      }
      if (infix.op == "==")
      {
        return std::visit(eqInfixEval{}, lhs.group, rhs.group);
      }
      if (infix.op == "!=")
      {
        return std::visit(neqInfixEval{}, lhs.group, rhs.group);
      }
      if (infix.op == ">")
      {
        return  std::visit(greatInfixEval{}, lhs.group, rhs.group);
      }
      if (infix.op == "<")
      {
        return  std::visit(lessInfixEval{}, lhs.group, rhs.group);
      }
      return Object::NullObject{};
    }

    struct bangPrefixEval : defaultErrorVisitor<bangPrefixEval>
    {
      constexpr static std::string_view Operator = "!";
      using defaultErrorVisitor::operator();
      Object::Object operator()(const Object::IntegerObject &def) { return Object::BoolObject{!static_cast<bool>(def.Value)}; };
      Object::Object operator()(const Object::BoolObject &def) { return Object::BoolObject{!def.Value}; };
      Object::Object operator()([[maybe_unused]] const Object::NullObject &rhs) { return Object::BoolObject{true}; };
      Object::Object operator()(Object::ReturnObject lhs) { return std::visit(*this, lhs.Value->group); };
    };
    struct subPrefixEval : defaultErrorVisitor<subPrefixEval>
    {
      constexpr static std::string_view Operator = "-";
      using defaultErrorVisitor::operator();
      Object::Object operator()(const Object::IntegerObject &rhs){ return Object::IntegerObject{- static_cast<std::int64_t>(rhs.Value) }; };
      Object::Object operator()(Object::ReturnObject lhs) { return std::visit(*this, lhs.Value->group); }
    };

    struct addPrefixEval: defaultErrorVisitor<addPrefixEval>
    {
      constexpr static std::string_view Operator = "+";
      using defaultErrorVisitor::operator();
      Object::Object operator()(const Object::IntegerObject &rhs) { return rhs; };
      Object::Object operator()(Object::ReturnObject lhs) { return std::visit(*this, lhs.Value->group); }
    };

    [[nodiscard]] Object::Object operator()(const ast::PrefixExpression &prefix)
    {
      auto right = (*this)(*prefix.right);
      // Unary ! converts ints to bools and nots them, and promotes nulls to bool (and nots them to true)
      if (prefix.op == "!") {
        return std::visit(bangPrefixEval{}, right.group);
      }
      // unary - does c++ things to ints and bools and promotes Nulls to int{0}
      if (prefix.op == "-") {
        return std::visit(subPrefixEval{}, right.group) ;
      }
      // Unary + promotes bools to ints, and nulls to 0
      if (prefix.op == "+") {
        return std::visit(addPrefixEval{}, right.group) ;
      }
      // TODO: ERROR?
      return right;
    }

    [[nodiscard]] Object::Object operator()(ast::IfExpression ifExpression)
    {
      if (ifExpression.Condition != nullptr)
      {
        auto condition = (*this)(*ifExpression.Condition);

        // bubble errors up:
        if (std::holds_alternative<Object::ErrorObject>(condition.group)) { return condition; }

        if (IsTruthy(condition))
        {
          if (ifExpression.Consequence.has_value())
          {
            return (*this)(ifExpression.Consequence.value());
          }
          return Object::ErrorObject{ "IfExpression holds no Consequence" };
        }
        else
        {
          if (ifExpression.Alternative.has_value())
          {
            return (*this)(ifExpression.Alternative.value());
          }
          // TODO: return an ErrorObject
          // TODO: confirm consequence and Alternative have the same type?
          return Object::NullObject{};
        }
      }
      return Object::ErrorObject{ "IfExpression holds no Condition" };
    }

    [[nodiscard]] std::vector<Object::Object> evalExpressions(std::vector<ast::Expression>* exprs)
    {
      std::vector<Object::Object> results;
      for (const auto& expr : *exprs)
      {
        auto res = (*this)(expr);
        // bubble errors up:
        if (std::holds_alternative<Object::ErrorObject>(res.group)) { return std::vector<Object::Object>{ res }; }
        results.push_back(res);
      }
      return results;
    }

    [[nodiscard]] std::vector<std::pair<Object::Object, Object::Object>> evalExpressionPairs(std::vector<std::pair<ast::Expression, ast::Expression> >* exprs)
    {
      std::vector<std::pair<Object::Object, Object::Object>> results;
      for (const auto& [lhs, rhs] : *exprs)
      {
        auto res_lhs = (*this)(lhs);
        auto res_rhs = (*this)(rhs);
        // bubble errors up:
        if (std::holds_alternative<Object::ErrorObject>(res_lhs.group) || std::holds_alternative<Object::ErrorObject>(res_rhs.group)) { return { { res_lhs, res_rhs } }; }
        results.push_back({res_lhs, res_rhs});
      }
      return results;
    }

    // TODO: callExpression can rather use a variant in it for <Identifier, FunctionLiteral> instead of an expression pointer!
    [[nodiscard]] Object::Object operator()(const ast::CallExpression &callExpression)
    {
      // this will either evaluate an identifier, or a functionLiteral to a functionObject
      auto retObject = (*this)(*callExpression.function);
      // bubble errors up:
      if (std::holds_alternative<Object::ErrorObject>(retObject.group)) { return retObject; }

      if (Object::FunctionObject* funcObjectPtr = std::get_if<Object::FunctionObject>(&retObject.group); funcObjectPtr != nullptr)
      {
        Object::FunctionObject& funcObject = *funcObjectPtr;
        // evaluate each parameter to the function call
        auto arg_results = evalExpressions(callExpression.arguments);

        // bubble errors up:
        if (arg_results.size() == 1)
        {
          if (std::holds_alternative<Object::ErrorObject>(arg_results[0].group)) { return arg_results[0]; }
        }

        if(funcObject.numParms >= 0 && static_cast<std::size_t>(funcObject.numParms) != arg_results.size())
        {
          return Object::ErrorObject{std::string{"function called with wrong number of args! expected: "}+std::to_string(funcObject.numParms) + std::string{", got: "} + std::to_string(arg_results.size())};
        }

        if (funcObject.body.has_value())
        {
          // extend the function's env and push it on the stack:
          EnvPushPopper eppf(*this, getNewEnv(funcObjectPtr->Env));

          for (const auto &[val_obj, identifier] : std::views::zip(arg_results, *funcObject.parameters))
          {
            addVar(identifier.Value, val_obj);
          }
          auto evalled = evalBlockStatement(funcObject.body.value(),getEnv());// funcObject.Env);
          // unwrap returnObject
          if (auto *ret = std::get_if<Object::ReturnObject>(&evalled.group); ret != nullptr)
          {
            return *ret->Value;
          }
          if (std::holds_alternative<Object::ErrorObject>(evalled.group)) { return evalled; }
          return evalled;
        }
        else if (funcObject.builtin.has_value())
        {
          if(auto* fptr = funcObject.builtin.value();fptr!=nullptr)
          {
            return (*funcObject.builtin.value())(arg_results);
          }
          return Object::ErrorObject{"Builtin function has null as function pointer"};
        }
        return Object::ErrorObject{ "Function Literal with Empty Body" };
      }
      return Object::ErrorObject{ "CallExpression function did not resolve to a FunctionObject" };
    }

    [[nodiscard]] Object::Object operator()(const ast::FunctionLiteral &functionLiteral)
    {
      return Object::FunctionObject{ functionLiteral.parameters, static_cast<std::int8_t>(functionLiteral.parameters->size()), functionLiteral.body, std::nullopt, getEnv()};
    }

    [[nodiscard]] Object::Object operator()(const ast::ArrayLiteral &array)
    {
      auto elems = evalExpressions(array.elements);
      // bubble errors up:
      if (elems.size() == 1)
      {
        if (std::holds_alternative<Object::ErrorObject>(elems[0].group)) { return elems[0]; }
      }
      return Object::ArrayObject{ elems };
    }

    [[nodiscard]] Object::Object operator()(const ast::HashLiteral &hash)
    {
      auto elems = evalExpressionPairs(hash.pairs);
      // bubble errors up:
      if (elems.size() == 1)
      {
        if (std::holds_alternative<Object::ErrorObject>(elems[0].first.group)) { return elems[0].first; }
        if (std::holds_alternative<Object::ErrorObject>(elems[0].second.group)) { return elems[0].second; }
      }

      Object::HashObject out;
      for(auto&& elem : elems)
      {
        out.Value[elem.first.Hash()]=std::move(elem);
      }

      return out;
    }

    [[nodiscard]] Object::Object operator()(const ast::Expression &expr)
    {
      return std::visit(*this, expr.group);
    }

    [[nodiscard]] Object::Object operator()(const ast::IndexExpression &indexExpr)
    {
      auto left = (*this)(*indexExpr.left);
      if (std::holds_alternative<Object::ErrorObject>(left.group)) { return left;};
      auto index = (*this)(*indexExpr.index);
      if (std::holds_alternative<Object::ErrorObject>(index.group)) { return index;};
      if (std::holds_alternative<Object::ArrayObject>(left.group) && std::holds_alternative<Object::IntegerObject>(index.group))
      {
        const auto& arr = std::get_if<Object::ArrayObject>(&left.group)->Value;
        auto idx = std::get_if<Object::IntegerObject>(&index.group)->Value;
        if (idx < 0 || static_cast<std::uint64_t>(idx) >= arr.size())
        {
          // TODO: null, or error?
          return Object::NullObject{};
        }
      // return arr[static_cast<unsigned long>(idx)];
      return arr[idx];
      }
      if (const auto* hashObject = std::get_if<Object::HashObject>(&left.group); hashObject != nullptr)
      {
        if(std::holds_alternative<Object::ErrorObject>(index.group) || std::holds_alternative<Object::FunctionObject>(index.group))
        {
          return Object::ErrorObject{std::format("index of type {} not supported on hash", index.getTypeString())};
        }
        const auto& hash = hashObject ->Value;
        if (const auto& val = hash.find(index.Hash()); val!=hash.end())
        {
          return val->second.second;
        }
        // TODO: null, or error?
        return Object::NullObject{};
      }
      return Object::ErrorObject{std::format("index operator not supported for: {} and index: {}", left.getTypeString(), index.getTypeString())};
    }

    [[nodiscard]] Object::Object operator()(const ast::BlockStatement &blockStatement)
    {
      return evalBlockStatement(blockStatement, getNewEnv(getEnv()));
    }

    [[nodiscard]] Object::Object evalBlockStatement(const ast::BlockStatement& blockStatement, Environment* env)
    {
      EnvPushPopper epp{ *this, env };
      auto result = Object::Object{};
      if (blockStatement.statements != nullptr) 
      { 
        for (const auto &statement : *blockStatement.statements)
        {
          result = (*this)(statement);
          if (auto *ret = std::get_if<Object::ReturnObject>(&result.group); ret != nullptr)
          {
            return *ret;
          }
          // bubble errors up:
          if (std::holds_alternative<Object::ErrorObject>(result.group)) { return result; }
        }
      }
      return result;
    }

    [[nodiscard]] Object::Object operator()(ast::ReturnStatement returnStatement)
    {
      if (returnStatement.ReturnValue.has_value()) {
        auto result = (*this)(returnStatement.ReturnValue.value());
        // bubble errors up:
        if (std::holds_alternative<Object::ErrorObject>(result.group)) { return result; }
        return Object::ReturnObject{ getNewObject(result) };
      }
      // TODO: return Object::ErrorObject?
      return Object::NullObject{};
    }

    [[nodiscard]] Object::Object operator()(ast::ExpressionStatement exprStatement)
    {
      if (exprStatement.expression.has_value()) { 
        auto result = (*this)(exprStatement.expression.value());
        // bubble errors up:
        if (std::holds_alternative<Object::ErrorObject>(result.group)) { return result; }
        return result; 
      }
      // TODO: expression should really not be std::nullopt...
      // TODO: return Object::ErrorObject?
      return Object::NullObject{};
    }
    [[nodiscard]] Object::Object operator()(ast::LetStatement letStatement)
    {
      if (letStatement.Value.has_value()) { 
        auto result = (*this)(letStatement.Value.value());
        // bubble errors up:
        if (std::holds_alternative<Object::ErrorObject>(result.group)) { return result; }
        addVar(letStatement.Name.Value, result);
        return result; 
      }
      // TODO: return Object::ErrorObject?
      return Object::NullObject{};

    }

    [[nodiscard]] Object::Object operator()(ast::Statement statement)
    {
      return std::visit(*this, statement.group);
    }

    [[nodiscard]] Object::Object operator()(const ast::Program &program)
    {
      auto result = Object::Object{};
      for (const auto& statement : program.statements)
      {
        result = (*this)(statement);
        // unwrap returnObject:
        if (auto* ret = std::get_if<Object::ReturnObject>(&result.group); ret != nullptr)
        {
          return *ret->Value;
        }
        // bubble errors up:
        if (std::holds_alternative<Object::ErrorObject>(result.group)) 
        {
          return result;
        }
      }
      return result;
    }

    [[nodiscard]] Object::Object EvaluateProgram(const ast::Program &program)
    {
      EnvPushPopper epp{ *this };
      return(*this)(program);
    }
  
    void registerBuiltin(std::string_view name, std::int8_t numParms, const std::function<Object::BuiltinFunctionType>& func)
    {
      BuiltinFunctions.push_back(func);
      getEnv()->Add(name, Object::FunctionObject{nullptr, numParms, std::nullopt, &BuiltinFunctions.back(), getEnv()});
    }

    void registerBuiltin(std::string_view name, const Object::Object& val)
    {
      getEnv()->Add(name, val);
    }
  };
}

