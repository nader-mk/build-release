#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/Program.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/Triple.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/Support/DynamicLibrary.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <memory>
#include <cstdint>
#include <cstdlib>

enum class TokenType {
    FUNC,
    RETURN,
    LET,
    INT_TYPE,
    LONG_TYPE,
    FLOAT_TYPE,
    VOID_TYPE,
    STRING_TYPE,
    PRINT,
    INPUT,
    WHILE,
    FOR,
    IF,
    ELSE,
    BREAK,
    CONTINUE,
    IDENT,
    INT_LIT,
    FLOAT_LIT,
    STRING_LIT,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    COLON,
    SEMICOLON,
    COMMA,
    PLUS,
    MINUS,
    STAR,
    SLASH,
    PERCENT,
    ASSIGN,
    PLUS_ASSIGN,
    MINUS_ASSIGN,
    STAR_ASSIGN,
    SLASH_ASSIGN,
    PERCENT_ASSIGN,
    INCREMENT,
    DECREMENT,
    LT,
    GT,
    LE,
    GE,
    EQ,
    NE,
    AND,
    OR,
    NOT,
    END_OF_FILE,
    UNKNOWN
};

struct Token {
    TokenType type;
    std::string value;
    int line;
    int col;
};

class Lexer {
public:
    Lexer(const std::string& src) : source(src), pos(0), line(1), col(1) {}

    std::vector<Token> tokenize() {
        std::vector<Token> tokens;
        while (pos < source.size()) {
            skipWhitespaceAndComments();
            if (pos >= source.size()) break;
            Token tok = nextToken();
            tokens.push_back(tok);
        }
        tokens.push_back({TokenType::END_OF_FILE, "", line, col});
        return tokens;
    }

private:
    std::string source;
    size_t pos;
    int line;
    int col;

    void skipWhitespaceAndComments() {
        while (pos < source.size()) {
            unsigned char c = source[pos];
            if (c == ' ' || c == '\t' || c == '\r') {
                pos++;
                col++;
            } else if (c == '\n') {
                pos++;
                line++;
                col = 1;
            } else if (c == '/' && pos + 1 < source.size() && source[pos + 1] == '/') {
                while (pos < source.size() && source[pos] != '\n') {
                    pos++;
                }
            } else if (c == '/' && pos + 1 < source.size() && source[pos + 1] == '*') {
                pos += 2;
                col += 2;
                while (pos + 1 < source.size() && !(source[pos] == '*' && source[pos + 1] == '/')) {
                    if (source[pos] == '\n') {
                        line++;
                        col = 1;
                    } else {
                        col++;
                    }
                    pos++;
                }
                if (pos + 1 < source.size()) {
                    pos += 2;
                    col += 2;
                }
            } else {
                break;
            }
        }
    }

    uint32_t peekCodepoint() {
        if (pos >= source.size()) return 0;
        unsigned char c = source[pos];
        if ((c & 0x80) == 0) return c;
        if ((c & 0xE0) == 0xC0 && pos + 1 < source.size()) {
            return ((c & 0x1F) << 6) | (source[pos + 1] & 0x3F);
        }
        if ((c & 0xF0) == 0xE0 && pos + 2 < source.size()) {
            return ((c & 0x0F) << 12) | ((source[pos + 1] & 0x3F) << 6) | (source[pos + 2] & 0x3F);
        }
        if ((c & 0xF8) == 0xF0 && pos + 3 < source.size()) {
            return ((c & 0x07) << 18) | ((source[pos + 1] & 0x3F) << 12) | ((source[pos + 2] & 0x3F) << 6) | (source[pos + 3] & 0x3F);
        }
        return c;
    }

    size_t currentCharLen() {
        if (pos >= source.size()) return 0;
        unsigned char c = source[pos];
        if ((c & 0x80) == 0) return 1;
        if ((c & 0xE0) == 0xC0) return 2;
        if ((c & 0xF0) == 0xE0) return 3;
        if ((c & 0xF8) == 0xF0) return 4;
        return 1;
    }

    void advance() {
        size_t len = currentCharLen();
        pos += len;
        col++;
    }

    std::string readChar() {
        size_t len = currentCharLen();
        std::string s = source.substr(pos, len);
        pos += len;
        col++;
        return s;
    }

    bool isBanglaLetter(uint32_t cp) {
        return (cp >= 0x0980 && cp <= 0x09FF);
    }

    bool isBanglaDigit(uint32_t cp) {
        return (cp >= 0x09E6 && cp <= 0x09EF);
    }

    bool isAsciiDigit(uint32_t cp) {
        return (cp >= '0' && cp <= '9');
    }

    bool isAsciiLetter(uint32_t cp) {
        return (cp >= 'a' && cp <= 'z') || (cp >= 'A' && cp <= 'Z') || cp == '_';
    }

    bool isIdentStart(uint32_t cp) {
        return isBanglaLetter(cp) || isAsciiLetter(cp);
    }

    bool isIdentPart(uint32_t cp) {
        return isBanglaLetter(cp) || isAsciiLetter(cp) || isAsciiDigit(cp) || isBanglaDigit(cp);
    }

    bool isDigitStart(uint32_t cp) {
        return isAsciiDigit(cp) || isBanglaDigit(cp);
    }

    char normalizeBanglaDigit(uint32_t cp) {
        if (isBanglaDigit(cp)) {
            return '0' + (cp - 0x09E6);
        }
        return (char)cp;
    }

    Token nextToken() {
        int startLine = line;
        int startCol = col;

        uint32_t cp = peekCodepoint();

        if (cp == '"') {
            advance();
            std::string str;
            while (pos < source.size()) {
                cp = peekCodepoint();
                if (cp == '"') {
                    advance();
                    break;
                }
                if (cp == '\\' && pos + 1 < source.size()) {
                    advance();
                    cp = peekCodepoint();
                    if (cp == 'n') {
                        str += '\n';
                        advance();
                    } else if (cp == 't') {
                        str += '\t';
                        advance();
                    } else if (cp == '\\') {
                        str += '\\';
                        advance();
                    } else if (cp == '"') {
                        str += '"';
                        advance();
                    } else {
                        str += readChar();
                    }
                } else {
                    str += readChar();
                }
            }
            return {TokenType::STRING_LIT, str, startLine, startCol};
        }

        if (cp == '(') { advance(); return {TokenType::LPAREN, "(", startLine, startCol}; }
        if (cp == ')') { advance(); return {TokenType::RPAREN, ")", startLine, startCol}; }
        if (cp == '{') { advance(); return {TokenType::LBRACE, "{", startLine, startCol}; }
        if (cp == '}') { advance(); return {TokenType::RBRACE, "}", startLine, startCol}; }
        if (cp == '[') { advance(); return {TokenType::LBRACKET, "[", startLine, startCol}; }
        if (cp == ']') { advance(); return {TokenType::RBRACKET, "]", startLine, startCol}; }
        if (cp == ':') { advance(); return {TokenType::COLON, ":", startLine, startCol}; }
        if (cp == ';') { advance(); return {TokenType::SEMICOLON, ";", startLine, startCol}; }
        if (cp == ',') { advance(); return {TokenType::COMMA, ",", startLine, startCol}; }

        if (cp == '+') {
            advance();
            if (peekCodepoint() == '+') {
                advance();
                return {TokenType::INCREMENT, "++", startLine, startCol};
            }
            if (peekCodepoint() == '=') {
                advance();
                return {TokenType::PLUS_ASSIGN, "+=", startLine, startCol};
            }
            return {TokenType::PLUS, "+", startLine, startCol};
        }

        if (cp == '-') {
            advance();
            if (peekCodepoint() == '-') {
                advance();
                return {TokenType::DECREMENT, "--", startLine, startCol};
            }
            if (peekCodepoint() == '=') {
                advance();
                return {TokenType::MINUS_ASSIGN, "-=", startLine, startCol};
            }
            return {TokenType::MINUS, "-", startLine, startCol};
        }

        if (cp == '*') {
            advance();
            if (peekCodepoint() == '=') {
                advance();
                return {TokenType::STAR_ASSIGN, "*=", startLine, startCol};
            }
            return {TokenType::STAR, "*", startLine, startCol};
        }

        if (cp == '/') {
            advance();
            if (peekCodepoint() == '=') {
                advance();
                return {TokenType::SLASH_ASSIGN, "/=", startLine, startCol};
            }
            return {TokenType::SLASH, "/", startLine, startCol};
        }

        if (cp == '%') {
            advance();
            if (peekCodepoint() == '=') {
                advance();
                return {TokenType::PERCENT_ASSIGN, "%=", startLine, startCol};
            }
            return {TokenType::PERCENT, "%", startLine, startCol};
        }

        if (cp == '<') {
            advance();
            if (peekCodepoint() == '=') {
                advance();
                return {TokenType::LE, "<=", startLine, startCol};
            }
            return {TokenType::LT, "<", startLine, startCol};
        }

        if (cp == '>') {
            advance();
            if (peekCodepoint() == '=') {
                advance();
                return {TokenType::GE, ">=", startLine, startCol};
            }
            return {TokenType::GT, ">", startLine, startCol};
        }

        if (cp == '=') {
            advance();
            if (peekCodepoint() == '=') {
                advance();
                return {TokenType::EQ, "==", startLine, startCol};
            }
            return {TokenType::ASSIGN, "=", startLine, startCol};
        }

        if (cp == '!') {
            advance();
            if (peekCodepoint() == '=') {
                advance();
                return {TokenType::NE, "!=", startLine, startCol};
            }
            return {TokenType::NOT, "!", startLine, startCol};
        }

        if (cp == '&') {
            advance();
            if (peekCodepoint() == '&') {
                advance();
                return {TokenType::AND, "&&", startLine, startCol};
            }
            return {TokenType::UNKNOWN, "&", startLine, startCol};
        }

        if (cp == '|') {
            advance();
            if (peekCodepoint() == '|') {
                advance();
                return {TokenType::OR, "||", startLine, startCol};
            }
            return {TokenType::UNKNOWN, "|", startLine, startCol};
        }

        if (isDigitStart(cp)) {
            std::string numStr;
            bool hasDecimal = false;
            while (pos < source.size()) {
                cp = peekCodepoint();
                if (isAsciiDigit(cp)) {
                    numStr += (char)cp;
                    advance();
                } else if (isBanglaDigit(cp)) {
                    numStr += normalizeBanglaDigit(cp);
                    advance();
                } else if (cp == '.' && !hasDecimal) {
                    numStr += '.';
                    hasDecimal = true;
                    advance();
                } else {
                    break;
                }
            }
            if (hasDecimal) {
                return {TokenType::FLOAT_LIT, numStr, startLine, startCol};
            }
            return {TokenType::INT_LIT, numStr, startLine, startCol};
        }

        if (isIdentStart(cp)) {
            std::string ident;
            while (pos < source.size()) {
                cp = peekCodepoint();
                if (isIdentPart(cp)) {
                    ident += readChar();
                } else {
                    break;
                }
            }

            if (ident == "ফাংশন") return {TokenType::FUNC, ident, startLine, startCol};
            if (ident == "ফেরত") return {TokenType::RETURN, ident, startLine, startCol};
            if (ident == "ধরি") return {TokenType::LET, ident, startLine, startCol};
            if (ident == "পূর্ণ") return {TokenType::INT_TYPE, ident, startLine, startCol};
            if (ident == "দীর্ঘ") return {TokenType::LONG_TYPE, ident, startLine, startCol};
            if (ident == "দশমিক") return {TokenType::FLOAT_TYPE, ident, startLine, startCol};
            if (ident == "শূন্য") return {TokenType::VOID_TYPE, ident, startLine, startCol};
            if (ident == "স্ট্রিং") return {TokenType::STRING_TYPE, ident, startLine, startCol};
            if (ident == "ছাপাও") return {TokenType::PRINT, ident, startLine, startCol};
            if (ident == "ইনপুট") return {TokenType::INPUT, ident, startLine, startCol};
            if (ident == "যতক্ষণ") return {TokenType::WHILE, ident, startLine, startCol};
            if (ident == "জন্য") return {TokenType::FOR, ident, startLine, startCol};
            if (ident == "যদি") return {TokenType::IF, ident, startLine, startCol};
            if (ident == "নাহলে") return {TokenType::ELSE, ident, startLine, startCol};
            if (ident == "থামো") return {TokenType::BREAK, ident, startLine, startCol};
            if (ident == "চালাও") return {TokenType::CONTINUE, ident, startLine, startCol};

            return {TokenType::IDENT, ident, startLine, startCol};
        }

        advance();
        return {TokenType::UNKNOWN, "", startLine, startCol};
    }
};

enum class TypeKind { INT, LONG, FLOAT, VOID, STRING };

struct ASTNode {
    virtual ~ASTNode() = default;
};

struct ExprNode : ASTNode {
    TypeKind exprType = TypeKind::INT;
};

struct IntLitExpr : ExprNode {
    int64_t value;
    IntLitExpr(int64_t v) : value(v) { exprType = TypeKind::INT; }
};

struct FloatLitExpr : ExprNode {
    double value;
    FloatLitExpr(double v) : value(v) { exprType = TypeKind::FLOAT; }
};

struct StringLitExpr : ExprNode {
    std::string value;
    StringLitExpr(const std::string& v) : value(v) { exprType = TypeKind::STRING; }
};

struct VarRefExpr : ExprNode {
    std::string name;
    VarRefExpr(const std::string& n) : name(n) {}
};

struct BinaryExpr : ExprNode {
    std::string op;
    std::unique_ptr<ExprNode> left;
    std::unique_ptr<ExprNode> right;
    BinaryExpr(const std::string& o, std::unique_ptr<ExprNode> l, std::unique_ptr<ExprNode> r)
        : op(o), left(std::move(l)), right(std::move(r)) {}
};

struct UnaryExpr : ExprNode {
    std::string op;
    std::unique_ptr<ExprNode> operand;
    UnaryExpr(const std::string& o, std::unique_ptr<ExprNode> e)
        : op(o), operand(std::move(e)) {}
};

struct ArrayAccessExpr : ExprNode {
    std::string arrayName;
    std::unique_ptr<ExprNode> index;
    ArrayAccessExpr(const std::string& name, std::unique_ptr<ExprNode> idx)
        : arrayName(name), index(std::move(idx)) {}
};

struct Array2DAccessExpr : ExprNode {
    std::string arrayName;
    std::unique_ptr<ExprNode> index1;
    std::unique_ptr<ExprNode> index2;
    Array2DAccessExpr(const std::string& name, std::unique_ptr<ExprNode> i1, std::unique_ptr<ExprNode> i2)
        : arrayName(name), index1(std::move(i1)), index2(std::move(i2)) {}
};

struct FunctionCallExpr : ExprNode {
    std::string funcName;
    std::vector<std::unique_ptr<ExprNode>> args;
    FunctionCallExpr(const std::string& name) : funcName(name) {}
};

struct InputExpr : ExprNode {
    TypeKind inputType = TypeKind::INT;
    InputExpr() { exprType = TypeKind::INT; }
};

struct PostfixExpr : ExprNode {
    std::string name;
    std::string op;
    PostfixExpr(const std::string& n, const std::string& o) : name(n), op(o) {}
};

struct StmtNode : ASTNode {};

struct VarDeclStmt : StmtNode {
    std::string name;
    TypeKind type;
    std::unique_ptr<ExprNode> init;
};

struct ArrayDeclStmt : StmtNode {
    std::string name;
    TypeKind elementType;
    int size;
    std::vector<std::unique_ptr<ExprNode>> initializer;
};

struct Array2DDeclStmt : StmtNode {
    std::string name;
    TypeKind elementType;
    int size1;
    int size2;
};

struct AssignStmt : StmtNode {
    std::string name;
    std::unique_ptr<ExprNode> value;
};

struct CompoundAssignStmt : StmtNode {
    std::string name;
    std::string op;
    std::unique_ptr<ExprNode> value;
};

struct ArrayAssignStmt : StmtNode {
    std::string arrayName;
    std::unique_ptr<ExprNode> index;
    std::unique_ptr<ExprNode> value;
};

struct Array2DAssignStmt : StmtNode {
    std::string arrayName;
    std::unique_ptr<ExprNode> index1;
    std::unique_ptr<ExprNode> index2;
    std::unique_ptr<ExprNode> value;
};

struct IncrementStmt : StmtNode {
    std::string name;
    std::string op;
};

struct ReturnStmt : StmtNode {
    std::unique_ptr<ExprNode> expr;
};

struct PrintStmt : StmtNode {
    std::unique_ptr<ExprNode> expr;
};

struct ExprStmt : StmtNode {
    std::unique_ptr<ExprNode> expr;
};

struct BlockStmt : StmtNode {
    std::vector<std::unique_ptr<StmtNode>> statements;
};

struct IfStmt : StmtNode {
    std::unique_ptr<ExprNode> condition;
    std::vector<std::unique_ptr<StmtNode>> thenBody;
    std::vector<std::unique_ptr<StmtNode>> elseBody;
};

struct WhileStmt : StmtNode {
    std::unique_ptr<ExprNode> condition;
    std::vector<std::unique_ptr<StmtNode>> body;
};

struct ForStmt : StmtNode {
    std::unique_ptr<StmtNode> init;
    std::unique_ptr<ExprNode> condition;
    std::unique_ptr<StmtNode> increment;
    std::vector<std::unique_ptr<StmtNode>> body;
};

struct BreakStmt : StmtNode {};

struct ContinueStmt : StmtNode {};

struct Parameter {
    std::string name;
    TypeKind type;
};

struct FunctionDecl : ASTNode {
    std::string name;
    TypeKind returnType;
    std::vector<Parameter> params;
    std::vector<std::unique_ptr<StmtNode>> body;
};

struct Program {
    std::vector<std::unique_ptr<FunctionDecl>> functions;
};

class Parser {
public:
    Parser(const std::vector<Token>& toks) : tokens(toks), pos(0) {}

    std::unique_ptr<Program> parse() {
        auto prog = std::make_unique<Program>();
        while (!isAtEnd()) {
            if (check(TokenType::FUNC)) {
                prog->functions.push_back(parseFunction());
            } else {
                advance();
            }
        }
        return prog;
    }

private:
    std::vector<Token> tokens;
    size_t pos;

    bool isAtEnd() { return peek().type == TokenType::END_OF_FILE; }
    Token peek() { return tokens[pos]; }
    Token peekNext() { return pos + 1 < tokens.size() ? tokens[pos + 1] : tokens[pos]; }
    Token peekAt(size_t offset) { return pos + offset < tokens.size() ? tokens[pos + offset] : tokens[tokens.size() - 1]; }
    Token previous() { return tokens[pos - 1]; }
    Token advance() { if (!isAtEnd()) pos++; return previous(); }
    bool check(TokenType t) { return peek().type == t; }

    bool match(TokenType t) {
        if (check(t)) { advance(); return true; }
        return false;
    }

    Token consume(TokenType t, const std::string& msg) {
        if (check(t)) return advance();
        std::cerr << "Parse error at line " << peek().line << ", col " << peek().col << ": " << msg << std::endl;
        exit(1);
    }

    TypeKind parseType() {
        if (match(TokenType::INT_TYPE)) return TypeKind::INT;
        if (match(TokenType::LONG_TYPE)) return TypeKind::LONG;
        if (match(TokenType::FLOAT_TYPE)) return TypeKind::FLOAT;
        if (match(TokenType::VOID_TYPE)) return TypeKind::VOID;
        if (match(TokenType::STRING_TYPE)) return TypeKind::STRING;
        std::cerr << "Expected type at line " << peek().line << std::endl;
        exit(1);
    }

    std::unique_ptr<FunctionDecl> parseFunction() {
        consume(TokenType::FUNC, "Expected ফাংশন");
        auto fn = std::make_unique<FunctionDecl>();
        Token name = consume(TokenType::IDENT, "Expected function name");
        fn->name = name.value;
        consume(TokenType::LPAREN, "Expected (");
        
        if (!check(TokenType::RPAREN)) {
            do {
                Token paramName = consume(TokenType::IDENT, "Expected parameter name");
                consume(TokenType::COLON, "Expected :");
                TypeKind paramType = parseType();
                fn->params.push_back({paramName.value, paramType});
            } while (match(TokenType::COMMA));
        }
        
        consume(TokenType::RPAREN, "Expected )");
        consume(TokenType::COLON, "Expected :");
        fn->returnType = parseType();
        consume(TokenType::LBRACE, "Expected {");
        while (!check(TokenType::RBRACE) && !isAtEnd()) {
            fn->body.push_back(parseStatement());
        }
        consume(TokenType::RBRACE, "Expected }");
        return fn;
    }

    std::unique_ptr<StmtNode> parseStatement() {
        if (match(TokenType::LET)) {
            return parseVarDecl();
        }
        if (match(TokenType::RETURN)) {
            return parseReturn();
        }
        if (match(TokenType::PRINT)) {
            return parsePrint();
        }
        if (match(TokenType::IF)) {
            return parseIf();
        }
        if (match(TokenType::WHILE)) {
            return parseWhile();
        }
        if (match(TokenType::FOR)) {
            return parseFor();
        }
        if (match(TokenType::BREAK)) {
            consume(TokenType::SEMICOLON, "Expected ;");
            return std::make_unique<BreakStmt>();
        }
        if (match(TokenType::CONTINUE)) {
            consume(TokenType::SEMICOLON, "Expected ;");
            return std::make_unique<ContinueStmt>();
        }
        if (match(TokenType::LBRACE)) {
            auto block = std::make_unique<BlockStmt>();
            while (!check(TokenType::RBRACE) && !isAtEnd()) {
                block->statements.push_back(parseStatement());
            }
            consume(TokenType::RBRACE, "Expected }");
            return block;
        }
        if (check(TokenType::IDENT)) {
            if (peekNext().type == TokenType::INCREMENT || peekNext().type == TokenType::DECREMENT) {
                Token name = advance();
                Token op = advance();
                consume(TokenType::SEMICOLON, "Expected ;");
                auto stmt = std::make_unique<IncrementStmt>();
                stmt->name = name.value;
                stmt->op = op.value;
                return stmt;
            }
            if (peekNext().type == TokenType::PLUS_ASSIGN || peekNext().type == TokenType::MINUS_ASSIGN ||
                peekNext().type == TokenType::STAR_ASSIGN || peekNext().type == TokenType::SLASH_ASSIGN ||
                peekNext().type == TokenType::PERCENT_ASSIGN) {
                Token name = advance();
                Token op = advance();
                auto value = parseExpression();
                consume(TokenType::SEMICOLON, "Expected ;");
                auto stmt = std::make_unique<CompoundAssignStmt>();
                stmt->name = name.value;
                stmt->op = op.value;
                stmt->value = std::move(value);
                return stmt;
            }
            if (peekNext().type == TokenType::ASSIGN) {
                Token name = advance();
                advance();
                auto value = parseExpression();
                consume(TokenType::SEMICOLON, "Expected ;");
                auto stmt = std::make_unique<AssignStmt>();
                stmt->name = name.value;
                stmt->value = std::move(value);
                return stmt;
            }
            if (peekNext().type == TokenType::LBRACKET) {
                Token name = advance();
                advance();
                auto index1 = parseExpression();
                consume(TokenType::RBRACKET, "Expected ]");
                
                if (check(TokenType::LBRACKET)) {
                    advance();
                    auto index2 = parseExpression();
                    consume(TokenType::RBRACKET, "Expected ]");
                    consume(TokenType::ASSIGN, "Expected =");
                    auto value = parseExpression();
                    consume(TokenType::SEMICOLON, "Expected ;");
                    auto stmt = std::make_unique<Array2DAssignStmt>();
                    stmt->arrayName = name.value;
                    stmt->index1 = std::move(index1);
                    stmt->index2 = std::move(index2);
                    stmt->value = std::move(value);
                    return stmt;
                }
                
                consume(TokenType::ASSIGN, "Expected =");
                auto value = parseExpression();
                consume(TokenType::SEMICOLON, "Expected ;");
                auto stmt = std::make_unique<ArrayAssignStmt>();
                stmt->arrayName = name.value;
                stmt->index = std::move(index1);
                stmt->value = std::move(value);
                return stmt;
            }
        }
        auto expr = parseExpression();
        consume(TokenType::SEMICOLON, "Expected ;");
        auto stmt = std::make_unique<ExprStmt>();
        stmt->expr = std::move(expr);
        return stmt;
    }

    std::unique_ptr<StmtNode> parseVarDecl() {
        Token name = consume(TokenType::IDENT, "Expected variable name");
        consume(TokenType::COLON, "Expected :");
        TypeKind type = parseType();
        
        if (match(TokenType::LBRACKET)) {
            Token sizeToken1 = consume(TokenType::INT_LIT, "Expected array size");
            consume(TokenType::RBRACKET, "Expected ]");
            
            if (match(TokenType::LBRACKET)) {
                Token sizeToken2 = consume(TokenType::INT_LIT, "Expected array size");
                consume(TokenType::RBRACKET, "Expected ]");
                consume(TokenType::SEMICOLON, "Expected ;");
                
                auto arrayStmt = std::make_unique<Array2DDeclStmt>();
                arrayStmt->name = name.value;
                arrayStmt->elementType = type;
                arrayStmt->size1 = std::stoi(sizeToken1.value);
                arrayStmt->size2 = std::stoi(sizeToken2.value);
                return arrayStmt;
            }
            
            auto arrayStmt = std::make_unique<ArrayDeclStmt>();
            arrayStmt->name = name.value;
            arrayStmt->elementType = type;
            arrayStmt->size = std::stoi(sizeToken1.value);
            
            if (match(TokenType::ASSIGN)) {
                consume(TokenType::LBRACE, "Expected {");
                if (!check(TokenType::RBRACE)) {
                    arrayStmt->initializer.push_back(parseExpression());
                    while (match(TokenType::COMMA)) {
                        arrayStmt->initializer.push_back(parseExpression());
                    }
                }
                consume(TokenType::RBRACE, "Expected }");
            }
            consume(TokenType::SEMICOLON, "Expected ;");
            return arrayStmt;
        }
        
        auto stmt = std::make_unique<VarDeclStmt>();
        stmt->name = name.value;
        stmt->type = type;
        if (match(TokenType::ASSIGN)) {
            stmt->init = parseExpression();
        }
        consume(TokenType::SEMICOLON, "Expected ;");
        return stmt;
    }

    std::unique_ptr<ReturnStmt> parseReturn() {
        auto stmt = std::make_unique<ReturnStmt>();
        if (!check(TokenType::SEMICOLON)) {
            stmt->expr = parseExpression();
        }
        consume(TokenType::SEMICOLON, "Expected ;");
        return stmt;
    }

    std::unique_ptr<PrintStmt> parsePrint() {
        auto stmt = std::make_unique<PrintStmt>();
        consume(TokenType::LPAREN, "Expected (");
        stmt->expr = parseExpression();
        consume(TokenType::RPAREN, "Expected )");
        consume(TokenType::SEMICOLON, "Expected ;");
        return stmt;
    }

    std::unique_ptr<IfStmt> parseIf() {
        auto stmt = std::make_unique<IfStmt>();
        consume(TokenType::LPAREN, "Expected (");
        stmt->condition = parseExpression();
        consume(TokenType::RPAREN, "Expected )");
        consume(TokenType::LBRACE, "Expected {");
        while (!check(TokenType::RBRACE) && !isAtEnd()) {
            stmt->thenBody.push_back(parseStatement());
        }
        consume(TokenType::RBRACE, "Expected }");
        
        if (match(TokenType::ELSE)) {
            consume(TokenType::LBRACE, "Expected {");
            while (!check(TokenType::RBRACE) && !isAtEnd()) {
                stmt->elseBody.push_back(parseStatement());
            }
            consume(TokenType::RBRACE, "Expected }");
        }
        return stmt;
    }

    std::unique_ptr<WhileStmt> parseWhile() {
        auto stmt = std::make_unique<WhileStmt>();
        consume(TokenType::LPAREN, "Expected (");
        stmt->condition = parseExpression();
        consume(TokenType::RPAREN, "Expected )");
        consume(TokenType::LBRACE, "Expected {");
        while (!check(TokenType::RBRACE) && !isAtEnd()) {
            stmt->body.push_back(parseStatement());
        }
        consume(TokenType::RBRACE, "Expected }");
        return stmt;
    }

    std::unique_ptr<ForStmt> parseFor() {
        auto stmt = std::make_unique<ForStmt>();
        consume(TokenType::LPAREN, "Expected (");
        
        if (match(TokenType::LET)) {
            stmt->init = parseVarDecl();
        } else if (!check(TokenType::SEMICOLON)) {
            if (check(TokenType::IDENT)) {
                if (peekNext().type == TokenType::ASSIGN) {
                    Token name = advance();
                    advance();
                    auto value = parseExpression();
                    consume(TokenType::SEMICOLON, "Expected ;");
                    auto assignStmt = std::make_unique<AssignStmt>();
                    assignStmt->name = name.value;
                    assignStmt->value = std::move(value);
                    stmt->init = std::move(assignStmt);
                } else {
                    auto expr = parseExpression();
                    consume(TokenType::SEMICOLON, "Expected ;");
                    auto exprStmt = std::make_unique<ExprStmt>();
                    exprStmt->expr = std::move(expr);
                    stmt->init = std::move(exprStmt);
                }
            } else {
                auto expr = parseExpression();
                consume(TokenType::SEMICOLON, "Expected ;");
                auto exprStmt = std::make_unique<ExprStmt>();
                exprStmt->expr = std::move(expr);
                stmt->init = std::move(exprStmt);
            }
        } else {
            consume(TokenType::SEMICOLON, "Expected ;");
        }
        
        if (!check(TokenType::SEMICOLON)) {
            stmt->condition = parseExpression();
        }
        consume(TokenType::SEMICOLON, "Expected ;");
        
        if (!check(TokenType::RPAREN)) {
            if (check(TokenType::IDENT)) {
                if (peekNext().type == TokenType::INCREMENT || peekNext().type == TokenType::DECREMENT) {
                    Token name = advance();
                    Token op = advance();
                    auto incrStmt = std::make_unique<IncrementStmt>();
                    incrStmt->name = name.value;
                    incrStmt->op = op.value;
                    stmt->increment = std::move(incrStmt);
                } else if (peekNext().type == TokenType::PLUS_ASSIGN || peekNext().type == TokenType::MINUS_ASSIGN ||
                           peekNext().type == TokenType::STAR_ASSIGN || peekNext().type == TokenType::SLASH_ASSIGN ||
                           peekNext().type == TokenType::PERCENT_ASSIGN) {
                    Token name = advance();
                    Token op = advance();
                    auto value = parseExpression();
                    auto compStmt = std::make_unique<CompoundAssignStmt>();
                    compStmt->name = name.value;
                    compStmt->op = op.value;
                    compStmt->value = std::move(value);
                    stmt->increment = std::move(compStmt);
                } else if (peekNext().type == TokenType::ASSIGN) {
                    Token name = advance();
                    advance();
                    auto value = parseExpression();
                    auto assignStmt = std::make_unique<AssignStmt>();
                    assignStmt->name = name.value;
                    assignStmt->value = std::move(value);
                    stmt->increment = std::move(assignStmt);
                } else {
                    auto expr = parseExpression();
                    auto exprStmt = std::make_unique<ExprStmt>();
                    exprStmt->expr = std::move(expr);
                    stmt->increment = std::move(exprStmt);
                }
            } else {
                auto expr = parseExpression();
                auto exprStmt = std::make_unique<ExprStmt>();
                exprStmt->expr = std::move(expr);
                stmt->increment = std::move(exprStmt);
            }
        }
        consume(TokenType::RPAREN, "Expected )");
        
        consume(TokenType::LBRACE, "Expected {");
        while (!check(TokenType::RBRACE) && !isAtEnd()) {
            stmt->body.push_back(parseStatement());
        }
        consume(TokenType::RBRACE, "Expected }");
        return stmt;
    }

    std::unique_ptr<ExprNode> parseExpression() {
        return parseOr();
    }

    std::unique_ptr<ExprNode> parseOr() {
        auto left = parseAnd();
        while (match(TokenType::OR)) {
            auto right = parseAnd();
            left = std::make_unique<BinaryExpr>("||", std::move(left), std::move(right));
        }
        return left;
    }

    std::unique_ptr<ExprNode> parseAnd() {
        auto left = parseEquality();
        while (match(TokenType::AND)) {
            auto right = parseEquality();
            left = std::make_unique<BinaryExpr>("&&", std::move(left), std::move(right));
        }
        return left;
    }

    std::unique_ptr<ExprNode> parseEquality() {
        auto left = parseComparison();
        while (check(TokenType::EQ) || check(TokenType::NE)) {
            std::string op = check(TokenType::EQ) ? "==" : "!=";
            advance();
            auto right = parseComparison();
            left = std::make_unique<BinaryExpr>(op, std::move(left), std::move(right));
        }
        return left;
    }

    std::unique_ptr<ExprNode> parseComparison() {
        auto left = parseAddSub();
        while (check(TokenType::LT) || check(TokenType::GT) || 
               check(TokenType::LE) || check(TokenType::GE)) {
            std::string op;
            if (check(TokenType::LT)) op = "<";
            else if (check(TokenType::GT)) op = ">";
            else if (check(TokenType::LE)) op = "<=";
            else op = ">=";
            advance();
            auto right = parseAddSub();
            left = std::make_unique<BinaryExpr>(op, std::move(left), std::move(right));
        }
        return left;
    }

    std::unique_ptr<ExprNode> parseAddSub() {
        auto left = parseMulDiv();
        while (check(TokenType::PLUS) || check(TokenType::MINUS)) {
            std::string op = check(TokenType::PLUS) ? "+" : "-";
            advance();
            auto right = parseMulDiv();
            left = std::make_unique<BinaryExpr>(op, std::move(left), std::move(right));
        }
        return left;
    }

    std::unique_ptr<ExprNode> parseMulDiv() {
        auto left = parseUnary();
        while (check(TokenType::STAR) || check(TokenType::SLASH) || check(TokenType::PERCENT)) {
            std::string op;
            if (check(TokenType::STAR)) op = "*";
            else if (check(TokenType::SLASH)) op = "/";
            else op = "%";
            advance();
            auto right = parseUnary();
            left = std::make_unique<BinaryExpr>(op, std::move(left), std::move(right));
        }
        return left;
    }

    std::unique_ptr<ExprNode> parseUnary() {
        if (match(TokenType::NOT)) {
            auto operand = parseUnary();
            return std::make_unique<UnaryExpr>("!", std::move(operand));
        }
        if (match(TokenType::MINUS)) {
            auto operand = parseUnary();
            return std::make_unique<UnaryExpr>("-", std::move(operand));
        }
        return parsePostfix();
    }

    std::unique_ptr<ExprNode> parsePostfix() {
        auto expr = parsePrimary();
        
        if (auto* varRef = dynamic_cast<VarRefExpr*>(expr.get())) {
            if (check(TokenType::INCREMENT) || check(TokenType::DECREMENT)) {
                std::string op = check(TokenType::INCREMENT) ? "++" : "--";
                advance();
                return std::make_unique<PostfixExpr>(varRef->name, op);
            }
        }
        
        return expr;
    }

    std::unique_ptr<ExprNode> parsePrimary() {
        if (match(TokenType::INT_LIT)) {
            return std::make_unique<IntLitExpr>(std::stoll(previous().value));
        }
        if (match(TokenType::FLOAT_LIT)) {
            return std::make_unique<FloatLitExpr>(std::stod(previous().value));
        }
        if (match(TokenType::STRING_LIT)) {
            return std::make_unique<StringLitExpr>(previous().value);
        }
        if (match(TokenType::INPUT)) {
            consume(TokenType::LPAREN, "Expected (");
            consume(TokenType::RPAREN, "Expected )");
            return std::make_unique<InputExpr>();
        }
        if (match(TokenType::IDENT)) {
            std::string name = previous().value;
            if (match(TokenType::LBRACKET)) {
                auto index1 = parseExpression();
                consume(TokenType::RBRACKET, "Expected ]");
                
                if (match(TokenType::LBRACKET)) {
                    auto index2 = parseExpression();
                    consume(TokenType::RBRACKET, "Expected ]");
                    return std::make_unique<Array2DAccessExpr>(name, std::move(index1), std::move(index2));
                }
                
                return std::make_unique<ArrayAccessExpr>(name, std::move(index1));
            }
            if (match(TokenType::LPAREN)) {
                auto call = std::make_unique<FunctionCallExpr>(name);
                if (!check(TokenType::RPAREN)) {
                    call->args.push_back(parseExpression());
                    while (match(TokenType::COMMA)) {
                        call->args.push_back(parseExpression());
                    }
                }
                consume(TokenType::RPAREN, "Expected )");
                return call;
            }
            return std::make_unique<VarRefExpr>(name);
        }
        if (match(TokenType::LPAREN)) {
            auto expr = parseExpression();
            consume(TokenType::RPAREN, "Expected )");
            return expr;
        }
        std::cerr << "Unexpected token in expression at line " << peek().line << std::endl;
        exit(1);
    }
};

struct ArrayInfo {
    llvm::Value* ptr;
    int size1;
    int size2;
    bool is2D;
};

class CodeGenerator {
public:
    CodeGenerator() : context(std::make_unique<llvm::LLVMContext>()), 
                      builder(*context), 
                      module(std::make_unique<llvm::Module>("banglascript", *context)) {
        breakStack = {};
        continueStack = {};
    }

    void generate(Program* prog) {
        declarePrintf();
        declareScanf();
        declareStdFunctions();
        for (auto& fn : prog->functions) {
            declareFunction(fn.get());
        }
        for (auto& fn : prog->functions) {
            generateFunction(fn.get());
        }
        generateMain(prog);
    }

    void printIR() {
        module->print(llvm::outs(), nullptr);
    }

    bool compileToExecutable(const std::string& outputPath) {
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();

        auto targetTripleStr = llvm::sys::getDefaultTargetTriple();
        llvm::Triple targetTriple(targetTripleStr);
        module->setTargetTriple(targetTriple);

        std::string error;
        auto target = llvm::TargetRegistry::lookupTarget(targetTripleStr, error);
        if (!target) {
            std::cerr << "Target lookup failed: " << error << std::endl;
            return false;
        }

        auto cpu = "generic";
        auto features = "";
        llvm::TargetOptions opt;
        auto targetMachine = target->createTargetMachine(
            targetTriple, cpu, features, opt,
            std::optional<llvm::Reloc::Model>(),
            std::optional<llvm::CodeModel::Model>(),
            llvm::CodeGenOptLevel::Default);

        module->setDataLayout(targetMachine->createDataLayout());

        std::string objPath = outputPath + ".o";
        std::error_code ec;
        llvm::raw_fd_ostream dest(objPath, ec, llvm::sys::fs::OF_None);
        if (ec) {
            std::cerr << "Could not open file: " << ec.message() << std::endl;
            return false;
        }

        llvm::legacy::PassManager pass;
        if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, llvm::CodeGenFileType::ObjectFile)) {
            std::cerr << "Cannot emit object file" << std::endl;
            return false;
        }

        pass.run(*module);
        dest.flush();
        dest.close();

        std::string exePath = outputPath;
#ifdef _WIN32
        exePath += ".exe";
#endif
        std::string gccCmd = "gcc \"" + objPath + "\" -o \"" + exePath + "\" -lm";
        int result = std::system(gccCmd.c_str());
        
        std::remove(objPath.c_str());
        
        return result == 0;
    }

    int runJIT() {
        llvm::InitializeNativeTarget();
        llvm::InitializeNativeTargetAsmPrinter();
        llvm::InitializeNativeTargetAsmParser();
        
        llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);

        std::string error;
        llvm::EngineBuilder engineBuilder(std::move(module));
        engineBuilder.setErrorStr(&error);
        engineBuilder.setEngineKind(llvm::EngineKind::JIT);
        
        std::unique_ptr<llvm::ExecutionEngine> engine(engineBuilder.create());
        if (!engine) {
            std::cerr << "Failed to create JIT engine: " << error << std::endl;
            return 1;
        }

        engine->finalizeObject();

        auto mainFn = engine->FindFunctionNamed("main");
        if (!mainFn) {
            std::cerr << "Could not find main function" << std::endl;
            return 1;
        }

        typedef int (*MainFnType)();
        MainFnType mainPtr = (MainFnType)engine->getPointerToFunction(mainFn);
        
        return mainPtr();
    }

    llvm::Module* getModule() { return module.get(); }

private:
    std::unique_ptr<llvm::LLVMContext> context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;
    std::map<std::string, llvm::AllocaInst*> namedValues;
    std::map<std::string, TypeKind> varTypes;
    std::map<std::string, ArrayInfo> arrayInfos;
    std::map<std::string, llvm::Function*> functions;
    std::map<std::string, TypeKind> functionReturnTypes;
    llvm::Function* printfFn = nullptr;
    llvm::Function* scanfFn = nullptr;
    llvm::Function* absFn = nullptr;
    llvm::Function* labsFn = nullptr;
    llvm::Function* fabsFn = nullptr;
    llvm::Function* sqrtFn = nullptr;
    llvm::Function* powFn = nullptr;
    std::vector<llvm::BasicBlock*> breakStack;
    std::vector<llvm::BasicBlock*> continueStack;
    TypeKind currentFunctionReturnType = TypeKind::INT;

    void declarePrintf() {
        std::vector<llvm::Type*> printfArgs;
        printfArgs.push_back(llvm::PointerType::get(*context, 0));
        auto printfType = llvm::FunctionType::get(builder.getInt32Ty(), printfArgs, true);
        printfFn = llvm::Function::Create(printfType, llvm::Function::ExternalLinkage, "printf", *module);
    }

    void declareScanf() {
        std::vector<llvm::Type*> scanfArgs;
        scanfArgs.push_back(llvm::PointerType::get(*context, 0));
        auto scanfType = llvm::FunctionType::get(builder.getInt32Ty(), scanfArgs, true);
        scanfFn = llvm::Function::Create(scanfType, llvm::Function::ExternalLinkage, "scanf", *module);
    }

    void declareStdFunctions() {
        auto absType = llvm::FunctionType::get(builder.getInt32Ty(), {builder.getInt32Ty()}, false);
        absFn = llvm::Function::Create(absType, llvm::Function::ExternalLinkage, "abs", *module);

        auto labsType = llvm::FunctionType::get(builder.getInt64Ty(), {builder.getInt64Ty()}, false);
        labsFn = llvm::Function::Create(labsType, llvm::Function::ExternalLinkage, "llabs", *module);

        auto fabsType = llvm::FunctionType::get(builder.getDoubleTy(), {builder.getDoubleTy()}, false);
        fabsFn = llvm::Function::Create(fabsType, llvm::Function::ExternalLinkage, "fabs", *module);

        auto sqrtType = llvm::FunctionType::get(builder.getDoubleTy(), {builder.getDoubleTy()}, false);
        sqrtFn = llvm::Function::Create(sqrtType, llvm::Function::ExternalLinkage, "sqrt", *module);

        auto powType = llvm::FunctionType::get(builder.getDoubleTy(), {builder.getDoubleTy(), builder.getDoubleTy()}, false);
        powFn = llvm::Function::Create(powType, llvm::Function::ExternalLinkage, "pow", *module);
    }

    llvm::Type* getLLVMType(TypeKind t) {
        switch (t) {
            case TypeKind::INT: return builder.getInt32Ty();
            case TypeKind::LONG: return builder.getInt64Ty();
            case TypeKind::FLOAT: return builder.getDoubleTy();
            case TypeKind::VOID: return builder.getVoidTy();
            case TypeKind::STRING: return llvm::PointerType::get(*context, 0);
        }
        return builder.getInt32Ty();
    }

    void declareFunction(FunctionDecl* fn) {
        llvm::Type* retType = getLLVMType(fn->returnType);
        std::vector<llvm::Type*> paramTypes;
        for (auto& param : fn->params) {
            paramTypes.push_back(getLLVMType(param.type));
        }
        auto funcType = llvm::FunctionType::get(retType, paramTypes, false);
        auto func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, fn->name, *module);
        functions[fn->name] = func;
        functionReturnTypes[fn->name] = fn->returnType;
    }

    void generateFunction(FunctionDecl* fn) {
        llvm::Function* func = functions[fn->name];
        if (!func) return;

        auto entry = llvm::BasicBlock::Create(*context, "entry", func);
        builder.SetInsertPoint(entry);

        namedValues.clear();
        varTypes.clear();
        arrayInfos.clear();
        currentFunctionReturnType = fn->returnType;

        size_t idx = 0;
        for (auto& arg : func->args()) {
            auto alloca = builder.CreateAlloca(arg.getType(), nullptr, fn->params[idx].name);
            builder.CreateStore(&arg, alloca);
            namedValues[fn->params[idx].name] = alloca;
            varTypes[fn->params[idx].name] = fn->params[idx].type;
            idx++;
        }

        for (auto& stmt : fn->body) {
            generateStatement(stmt.get());
        }

        if (!builder.GetInsertBlock()->getTerminator()) {
            if (fn->returnType == TypeKind::VOID) {
                builder.CreateRetVoid();
            } else if (fn->returnType == TypeKind::INT) {
                builder.CreateRet(builder.getInt32(0));
            } else if (fn->returnType == TypeKind::LONG) {
                builder.CreateRet(builder.getInt64(0));
            } else {
                builder.CreateRet(llvm::ConstantFP::get(builder.getDoubleTy(), 0.0));
            }
        }
    }

    void generateMain(Program* prog) {
        llvm::Function* shuruFn = module->getFunction("শুরু");
        if (!shuruFn) return;

        auto mainType = llvm::FunctionType::get(builder.getInt32Ty(), {}, false);
        auto mainFn = llvm::Function::Create(mainType, llvm::Function::ExternalLinkage, "main", *module);

        auto entry = llvm::BasicBlock::Create(*context, "entry", mainFn);
        builder.SetInsertPoint(entry);

        if (shuruFn->getReturnType()->isVoidTy()) {
            builder.CreateCall(shuruFn);
            builder.CreateRet(builder.getInt32(0));
        } else if (shuruFn->getReturnType()->isDoubleTy()) {
            auto result = builder.CreateCall(shuruFn);
            auto intResult = builder.CreateFPToSI(result, builder.getInt32Ty());
            builder.CreateRet(intResult);
        } else if (shuruFn->getReturnType()->isIntegerTy(64)) {
            auto result = builder.CreateCall(shuruFn);
            auto intResult = builder.CreateTrunc(result, builder.getInt32Ty());
            builder.CreateRet(intResult);
        } else {
            auto result = builder.CreateCall(shuruFn);
            builder.CreateRet(result);
        }
    }

    void generateStatement(StmtNode* stmt) {
        if (auto* varDecl = dynamic_cast<VarDeclStmt*>(stmt)) {
            auto type = getLLVMType(varDecl->type);
            auto alloca = builder.CreateAlloca(type, nullptr, varDecl->name);
            namedValues[varDecl->name] = alloca;
            varTypes[varDecl->name] = varDecl->type;
            if (varDecl->init) {
                auto val = generateExpression(varDecl->init.get(), varDecl->type);
                builder.CreateStore(val, alloca);
            } else {
                if (varDecl->type == TypeKind::INT) {
                    builder.CreateStore(builder.getInt32(0), alloca);
                } else if (varDecl->type == TypeKind::LONG) {
                    builder.CreateStore(builder.getInt64(0), alloca);
                } else if (varDecl->type == TypeKind::FLOAT) {
                    builder.CreateStore(llvm::ConstantFP::get(builder.getDoubleTy(), 0.0), alloca);
                }
            }
            return;
        }

        if (auto* arrayDecl = dynamic_cast<ArrayDeclStmt*>(stmt)) {
            llvm::Type* elemType = getLLVMType(arrayDecl->elementType);
            llvm::ArrayType* arrayType = llvm::ArrayType::get(elemType, arrayDecl->size);
            auto alloca = builder.CreateAlloca(arrayType, nullptr, arrayDecl->name);
            
            ArrayInfo info;
            info.ptr = alloca;
            info.size1 = arrayDecl->size;
            info.size2 = 0;
            info.is2D = false;
            arrayInfos[arrayDecl->name] = info;
            varTypes[arrayDecl->name] = arrayDecl->elementType;
            
            for (size_t i = 0; i < arrayDecl->initializer.size() && (int)i < arrayDecl->size; i++) {
                auto val = generateExpression(arrayDecl->initializer[i].get(), arrayDecl->elementType);
                auto gep = builder.CreateGEP(arrayType, alloca, {builder.getInt32(0), builder.getInt32(i)});
                builder.CreateStore(val, gep);
            }
            return;
        }

        if (auto* array2DDecl = dynamic_cast<Array2DDeclStmt*>(stmt)) {
            llvm::Type* elemType = getLLVMType(array2DDecl->elementType);
            llvm::ArrayType* innerType = llvm::ArrayType::get(elemType, array2DDecl->size2);
            llvm::ArrayType* arrayType = llvm::ArrayType::get(innerType, array2DDecl->size1);
            auto alloca = builder.CreateAlloca(arrayType, nullptr, array2DDecl->name);
            
            ArrayInfo info;
            info.ptr = alloca;
            info.size1 = array2DDecl->size1;
            info.size2 = array2DDecl->size2;
            info.is2D = true;
            arrayInfos[array2DDecl->name] = info;
            varTypes[array2DDecl->name] = array2DDecl->elementType;
            return;
        }

        if (auto* assignStmt = dynamic_cast<AssignStmt*>(stmt)) {
            auto it = namedValues.find(assignStmt->name);
            if (it != namedValues.end()) {
                auto val = generateExpression(assignStmt->value.get(), varTypes[assignStmt->name]);
                builder.CreateStore(val, it->second);
            }
            return;
        }

        if (auto* compAssign = dynamic_cast<CompoundAssignStmt*>(stmt)) {
            auto it = namedValues.find(compAssign->name);
            if (it != namedValues.end()) {
                TypeKind varType = varTypes[compAssign->name];
                llvm::Type* llvmType = getLLVMType(varType);
                auto currentVal = builder.CreateLoad(llvmType, it->second);
                auto operand = generateExpression(compAssign->value.get(), varType);
                
                llvm::Value* result = nullptr;
                bool isFloat = varType == TypeKind::FLOAT;
                
                if (compAssign->op == "+=") {
                    result = isFloat ? builder.CreateFAdd(currentVal, operand) : builder.CreateAdd(currentVal, operand);
                } else if (compAssign->op == "-=") {
                    result = isFloat ? builder.CreateFSub(currentVal, operand) : builder.CreateSub(currentVal, operand);
                } else if (compAssign->op == "*=") {
                    result = isFloat ? builder.CreateFMul(currentVal, operand) : builder.CreateMul(currentVal, operand);
                } else if (compAssign->op == "/=") {
                    result = isFloat ? builder.CreateFDiv(currentVal, operand) : builder.CreateSDiv(currentVal, operand);
                } else if (compAssign->op == "%=") {
                    result = isFloat ? builder.CreateFRem(currentVal, operand) : builder.CreateSRem(currentVal, operand);
                }
                
                if (result) {
                    builder.CreateStore(result, it->second);
                }
            }
            return;
        }

        if (auto* incrStmt = dynamic_cast<IncrementStmt*>(stmt)) {
            auto it = namedValues.find(incrStmt->name);
            if (it != namedValues.end()) {
                TypeKind varType = varTypes[incrStmt->name];
                llvm::Type* llvmType = getLLVMType(varType);
                auto currentVal = builder.CreateLoad(llvmType, it->second);
                
                llvm::Value* result = nullptr;
                if (varType == TypeKind::FLOAT) {
                    llvm::Value* one = llvm::ConstantFP::get(builder.getDoubleTy(), 1.0);
                    result = (incrStmt->op == "++") ? builder.CreateFAdd(currentVal, one) : builder.CreateFSub(currentVal, one);
                } else if (varType == TypeKind::LONG) {
                    llvm::Value* one = builder.getInt64(1);
                    result = (incrStmt->op == "++") ? builder.CreateAdd(currentVal, one) : builder.CreateSub(currentVal, one);
                } else {
                    llvm::Value* one = builder.getInt32(1);
                    result = (incrStmt->op == "++") ? builder.CreateAdd(currentVal, one) : builder.CreateSub(currentVal, one);
                }
                
                builder.CreateStore(result, it->second);
            }
            return;
        }

        if (auto* arrayAssign = dynamic_cast<ArrayAssignStmt*>(stmt)) {
            auto it = arrayInfos.find(arrayAssign->arrayName);
            if (it != arrayInfos.end()) {
                auto index = generateExpression(arrayAssign->index.get(), TypeKind::INT);
                auto val = generateExpression(arrayAssign->value.get(), varTypes[arrayAssign->arrayName]);
                llvm::Type* elemType = getLLVMType(varTypes[arrayAssign->arrayName]);
                llvm::ArrayType* arrayType = llvm::ArrayType::get(elemType, it->second.size1);
                auto gep = builder.CreateGEP(arrayType, it->second.ptr, {builder.getInt32(0), index});
                builder.CreateStore(val, gep);
            }
            return;
        }

        if (auto* array2DAssign = dynamic_cast<Array2DAssignStmt*>(stmt)) {
            auto it = arrayInfos.find(array2DAssign->arrayName);
            if (it != arrayInfos.end() && it->second.is2D) {
                auto index1 = generateExpression(array2DAssign->index1.get(), TypeKind::INT);
                auto index2 = generateExpression(array2DAssign->index2.get(), TypeKind::INT);
                auto val = generateExpression(array2DAssign->value.get(), varTypes[array2DAssign->arrayName]);
                
                llvm::Type* elemType = getLLVMType(varTypes[array2DAssign->arrayName]);
                llvm::ArrayType* innerType = llvm::ArrayType::get(elemType, it->second.size2);
                llvm::ArrayType* arrayType = llvm::ArrayType::get(innerType, it->second.size1);
                
                auto gep = builder.CreateGEP(arrayType, it->second.ptr, {builder.getInt32(0), index1, index2});
                builder.CreateStore(val, gep);
            }
            return;
        }

        if (auto* retStmt = dynamic_cast<ReturnStmt*>(stmt)) {
            if (retStmt->expr) {
                auto val = generateExpression(retStmt->expr.get(), currentFunctionReturnType);
                builder.CreateRet(val);
            } else {
                builder.CreateRetVoid();
            }
            return;
        }

        if (auto* printStmt = dynamic_cast<PrintStmt*>(stmt)) {
            auto val = generateExpression(printStmt->expr.get(), TypeKind::INT);
            llvm::Value* formatStr;
            if (val->getType()->isDoubleTy()) {
                formatStr = builder.CreateGlobalString("%f\n", "fmt");
            } else if (val->getType()->isIntegerTy(64)) {
                formatStr = builder.CreateGlobalString("%lld\n", "fmt");
            } else if (val->getType()->isPointerTy()) {
                formatStr = builder.CreateGlobalString("%s\n", "fmt");
            } else {
                formatStr = builder.CreateGlobalString("%d\n", "fmt");
            }
            builder.CreateCall(printfFn, {formatStr, val});
            return;
        }

        if (auto* ifStmt = dynamic_cast<IfStmt*>(stmt)) {
            generateIf(ifStmt);
            return;
        }

        if (auto* whileStmt = dynamic_cast<WhileStmt*>(stmt)) {
            generateWhile(whileStmt);
            return;
        }

        if (auto* forStmt = dynamic_cast<ForStmt*>(stmt)) {
            generateFor(forStmt);
            return;
        }

        if (auto* blockStmt = dynamic_cast<BlockStmt*>(stmt)) {
            for (auto& s : blockStmt->statements) {
                generateStatement(s.get());
            }
            return;
        }

        if (dynamic_cast<BreakStmt*>(stmt)) {
            if (!breakStack.empty()) {
                builder.CreateBr(breakStack.back());
                llvm::Function* func = builder.GetInsertBlock()->getParent();
                auto unreachable = llvm::BasicBlock::Create(*context, "after.break", func);
                builder.SetInsertPoint(unreachable);
            }
            return;
        }

        if (dynamic_cast<ContinueStmt*>(stmt)) {
            if (!continueStack.empty()) {
                builder.CreateBr(continueStack.back());
                llvm::Function* func = builder.GetInsertBlock()->getParent();
                auto unreachable = llvm::BasicBlock::Create(*context, "after.continue", func);
                builder.SetInsertPoint(unreachable);
            }
            return;
        }

        if (auto* exprStmt = dynamic_cast<ExprStmt*>(stmt)) {
            generateExpression(exprStmt->expr.get(), TypeKind::INT);
            return;
        }
    }

    void generateIf(IfStmt* stmt) {
        llvm::Function* func = builder.GetInsertBlock()->getParent();
        
        auto cond = generateExpression(stmt->condition.get(), TypeKind::INT);
        llvm::Value* condBool;
        if (cond->getType()->isDoubleTy()) {
            condBool = builder.CreateFCmpONE(cond, llvm::ConstantFP::get(builder.getDoubleTy(), 0.0), "ifcond");
        } else if (cond->getType()->isIntegerTy(64)) {
            condBool = builder.CreateICmpNE(cond, builder.getInt64(0), "ifcond");
        } else {
            condBool = builder.CreateICmpNE(cond, builder.getInt32(0), "ifcond");
        }
        
        llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(*context, "if.then", func);
        llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(*context, "if.else", func);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*context, "if.merge", func);
        
        builder.CreateCondBr(condBool, thenBB, elseBB);
        
        builder.SetInsertPoint(thenBB);
        for (auto& s : stmt->thenBody) {
            generateStatement(s.get());
        }
        if (!builder.GetInsertBlock()->getTerminator()) {
            builder.CreateBr(mergeBB);
        }
        
        builder.SetInsertPoint(elseBB);
        for (auto& s : stmt->elseBody) {
            generateStatement(s.get());
        }
        if (!builder.GetInsertBlock()->getTerminator()) {
            builder.CreateBr(mergeBB);
        }
        
        builder.SetInsertPoint(mergeBB);
    }

    void generateWhile(WhileStmt* stmt) {
        llvm::Function* func = builder.GetInsertBlock()->getParent();
        
        llvm::BasicBlock* condBB = llvm::BasicBlock::Create(*context, "while.cond", func);
        llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(*context, "while.body", func);
        llvm::BasicBlock* afterBB = llvm::BasicBlock::Create(*context, "while.after", func);
        
        breakStack.push_back(afterBB);
        continueStack.push_back(condBB);
        
        builder.CreateBr(condBB);
        
        builder.SetInsertPoint(condBB);
        auto cond = generateExpression(stmt->condition.get(), TypeKind::INT);
        llvm::Value* condBool;
        if (cond->getType()->isDoubleTy()) {
            condBool = builder.CreateFCmpONE(cond, llvm::ConstantFP::get(builder.getDoubleTy(), 0.0), "whilecond");
        } else if (cond->getType()->isIntegerTy(64)) {
            condBool = builder.CreateICmpNE(cond, builder.getInt64(0), "whilecond");
        } else {
            condBool = builder.CreateICmpNE(cond, builder.getInt32(0), "whilecond");
        }
        builder.CreateCondBr(condBool, bodyBB, afterBB);
        
        builder.SetInsertPoint(bodyBB);
        for (auto& s : stmt->body) {
            generateStatement(s.get());
        }
        if (!builder.GetInsertBlock()->getTerminator()) {
            builder.CreateBr(condBB);
        }
        
        breakStack.pop_back();
        continueStack.pop_back();
        
        builder.SetInsertPoint(afterBB);
    }

    void generateFor(ForStmt* stmt) {
        llvm::Function* func = builder.GetInsertBlock()->getParent();
        
        if (stmt->init) {
            generateStatement(stmt->init.get());
        }
        
        llvm::BasicBlock* condBB = llvm::BasicBlock::Create(*context, "for.cond", func);
        llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(*context, "for.body", func);
        llvm::BasicBlock* incrBB = llvm::BasicBlock::Create(*context, "for.incr", func);
        llvm::BasicBlock* afterBB = llvm::BasicBlock::Create(*context, "for.after", func);
        
        breakStack.push_back(afterBB);
        continueStack.push_back(incrBB);
        
        builder.CreateBr(condBB);
        
        builder.SetInsertPoint(condBB);
        if (stmt->condition) {
            auto cond = generateExpression(stmt->condition.get(), TypeKind::INT);
            llvm::Value* condBool;
            if (cond->getType()->isDoubleTy()) {
                condBool = builder.CreateFCmpONE(cond, llvm::ConstantFP::get(builder.getDoubleTy(), 0.0), "forcond");
            } else if (cond->getType()->isIntegerTy(64)) {
                condBool = builder.CreateICmpNE(cond, builder.getInt64(0), "forcond");
            } else {
                condBool = builder.CreateICmpNE(cond, builder.getInt32(0), "forcond");
            }
            builder.CreateCondBr(condBool, bodyBB, afterBB);
        } else {
            builder.CreateBr(bodyBB);
        }
        
        builder.SetInsertPoint(bodyBB);
        for (auto& s : stmt->body) {
            generateStatement(s.get());
        }
        if (!builder.GetInsertBlock()->getTerminator()) {
            builder.CreateBr(incrBB);
        }
        
        builder.SetInsertPoint(incrBB);
        if (stmt->increment) {
            generateStatement(stmt->increment.get());
        }
        builder.CreateBr(condBB);
        
        breakStack.pop_back();
        continueStack.pop_back();
        
        builder.SetInsertPoint(afterBB);
    }

    llvm::Value* generateExpression(ExprNode* expr, TypeKind expectedType) {
        if (auto* intLit = dynamic_cast<IntLitExpr*>(expr)) {
            if (expectedType == TypeKind::FLOAT) {
                return llvm::ConstantFP::get(builder.getDoubleTy(), (double)intLit->value);
            }
            if (expectedType == TypeKind::LONG) {
                                return builder.getInt64(intLit->value);
            }
            return builder.getInt32((int32_t)intLit->value);
        }

        if (auto* floatLit = dynamic_cast<FloatLitExpr*>(expr)) {
            return llvm::ConstantFP::get(builder.getDoubleTy(), floatLit->value);
        }

        if (auto* stringLit = dynamic_cast<StringLitExpr*>(expr)) {
            return builder.CreateGlobalString(stringLit->value, "str");
        }

        if (auto* inputExpr = dynamic_cast<InputExpr*>(expr)) {
            llvm::Type* type = getLLVMType(expectedType);
            auto alloca = builder.CreateAlloca(type, nullptr, "input.tmp");
            
            llvm::Value* formatStr;
            if (expectedType == TypeKind::FLOAT) {
                formatStr = builder.CreateGlobalString("%lf", "scanf.fmt");
            } else if (expectedType == TypeKind::LONG) {
                formatStr = builder.CreateGlobalString("%lld", "scanf.fmt");
            } else {
                formatStr = builder.CreateGlobalString("%d", "scanf.fmt");
            }
            
            builder.CreateCall(scanfFn, {formatStr, alloca});
            return builder.CreateLoad(type, alloca);
        }

        if (auto* varRef = dynamic_cast<VarRefExpr*>(expr)) {
            auto it = namedValues.find(varRef->name);
            if (it != namedValues.end()) {
                auto loaded = builder.CreateLoad(it->second->getAllocatedType(), it->second);
                varRef->exprType = varTypes[varRef->name];
                return loaded;
            }
            std::cerr << "Unknown variable: " << varRef->name << std::endl;
            exit(1);
        }

        if (auto* arrayAccess = dynamic_cast<ArrayAccessExpr*>(expr)) {
            auto it = arrayInfos.find(arrayAccess->arrayName);
            if (it != arrayInfos.end()) {
                auto index = generateExpression(arrayAccess->index.get(), TypeKind::INT);
                llvm::Type* elemType = getLLVMType(varTypes[arrayAccess->arrayName]);
                llvm::ArrayType* arrayType = llvm::ArrayType::get(elemType, it->second.size1);
                auto gep = builder.CreateGEP(arrayType, it->second.ptr, {builder.getInt32(0), index});
                return builder.CreateLoad(elemType, gep);
            }
            std::cerr << "Unknown array: " << arrayAccess->arrayName << std::endl;
            exit(1);
        }

        if (auto* array2DAccess = dynamic_cast<Array2DAccessExpr*>(expr)) {
            auto it = arrayInfos.find(array2DAccess->arrayName);
            if (it != arrayInfos.end() && it->second.is2D) {
                auto index1 = generateExpression(array2DAccess->index1.get(), TypeKind::INT);
                auto index2 = generateExpression(array2DAccess->index2.get(), TypeKind::INT);
                
                llvm::Type* elemType = getLLVMType(varTypes[array2DAccess->arrayName]);
                llvm::ArrayType* innerType = llvm::ArrayType::get(elemType, it->second.size2);
                llvm::ArrayType* arrayType = llvm::ArrayType::get(innerType, it->second.size1);
                
                auto gep = builder.CreateGEP(arrayType, it->second.ptr, {builder.getInt32(0), index1, index2});
                return builder.CreateLoad(elemType, gep);
            }
            std::cerr << "Unknown 2D array: " << array2DAccess->arrayName << std::endl;
            exit(1);
        }

        if (auto* postfixExpr = dynamic_cast<PostfixExpr*>(expr)) {
            auto it = namedValues.find(postfixExpr->name);
            if (it != namedValues.end()) {
                TypeKind varType = varTypes[postfixExpr->name];
                llvm::Type* llvmType = getLLVMType(varType);
                auto currentVal = builder.CreateLoad(llvmType, it->second);
                
                llvm::Value* newVal = nullptr;
                if (varType == TypeKind::FLOAT) {
                    llvm::Value* one = llvm::ConstantFP::get(builder.getDoubleTy(), 1.0);
                    newVal = (postfixExpr->op == "++") ? builder.CreateFAdd(currentVal, one) : builder.CreateFSub(currentVal, one);
                } else if (varType == TypeKind::LONG) {
                    llvm::Value* one = builder.getInt64(1);
                    newVal = (postfixExpr->op == "++") ? builder.CreateAdd(currentVal, one) : builder.CreateSub(currentVal, one);
                } else {
                    llvm::Value* one = builder.getInt32(1);
                    newVal = (postfixExpr->op == "++") ? builder.CreateAdd(currentVal, one) : builder.CreateSub(currentVal, one);
                }
                
                builder.CreateStore(newVal, it->second);
                return currentVal;
            }
            std::cerr << "Unknown variable: " << postfixExpr->name << std::endl;
            exit(1);
        }

        if (auto* callExpr = dynamic_cast<FunctionCallExpr*>(expr)) {
            if (callExpr->funcName == "সর্বোচ্চ" || callExpr->funcName == "max") {
                if (callExpr->args.size() == 2) {
                    auto left = generateExpression(callExpr->args[0].get(), expectedType);
                    auto right = generateExpression(callExpr->args[1].get(), expectedType);
                    
                    if (left->getType()->isDoubleTy() || right->getType()->isDoubleTy()) {
                        if (!left->getType()->isDoubleTy()) {
                            left = builder.CreateSIToFP(left, builder.getDoubleTy());
                        }
                        if (!right->getType()->isDoubleTy()) {
                            right = builder.CreateSIToFP(right, builder.getDoubleTy());
                        }
                        auto cmp = builder.CreateFCmpOGT(left, right);
                        return builder.CreateSelect(cmp, left, right);
                    } else {
                        auto cmp = builder.CreateICmpSGT(left, right);
                        return builder.CreateSelect(cmp, left, right);
                    }
                }
            }
            
            if (callExpr->funcName == "সর্বনিম্ন" || callExpr->funcName == "min") {
                if (callExpr->args.size() == 2) {
                    auto left = generateExpression(callExpr->args[0].get(), expectedType);
                    auto right = generateExpression(callExpr->args[1].get(), expectedType);
                    
                    if (left->getType()->isDoubleTy() || right->getType()->isDoubleTy()) {
                        if (!left->getType()->isDoubleTy()) {
                            left = builder.CreateSIToFP(left, builder.getDoubleTy());
                        }
                        if (!right->getType()->isDoubleTy()) {
                            right = builder.CreateSIToFP(right, builder.getDoubleTy());
                        }
                        auto cmp = builder.CreateFCmpOLT(left, right);
                        return builder.CreateSelect(cmp, left, right);
                    } else {
                        auto cmp = builder.CreateICmpSLT(left, right);
                        return builder.CreateSelect(cmp, left, right);
                    }
                }
            }
            
            if (callExpr->funcName == "পরম" || callExpr->funcName == "abs") {
                if (callExpr->args.size() == 1) {
                    auto arg = generateExpression(callExpr->args[0].get(), expectedType);
                    if (arg->getType()->isDoubleTy()) {
                        return builder.CreateCall(fabsFn, {arg});
                    } else if (arg->getType()->isIntegerTy(64)) {
                        return builder.CreateCall(labsFn, {arg});
                    } else {
                        return builder.CreateCall(absFn, {arg});
                    }
                }
            }
            
            if (callExpr->funcName == "বর্গমূল" || callExpr->funcName == "sqrt") {
                if (callExpr->args.size() == 1) {
                    auto arg = generateExpression(callExpr->args[0].get(), TypeKind::FLOAT);
                    if (!arg->getType()->isDoubleTy()) {
                        arg = builder.CreateSIToFP(arg, builder.getDoubleTy());
                    }
                    return builder.CreateCall(sqrtFn, {arg});
                }
            }
            
            if (callExpr->funcName == "ঘাত" || callExpr->funcName == "pow") {
                if (callExpr->args.size() == 2) {
                    auto base = generateExpression(callExpr->args[0].get(), TypeKind::FLOAT);
                    auto exp = generateExpression(callExpr->args[1].get(), TypeKind::FLOAT);
                    if (!base->getType()->isDoubleTy()) {
                        base = builder.CreateSIToFP(base, builder.getDoubleTy());
                    }
                    if (!exp->getType()->isDoubleTy()) {
                        exp = builder.CreateSIToFP(exp, builder.getDoubleTy());
                    }
                    return builder.CreateCall(powFn, {base, exp});
                }
            }
            
            auto it = functions.find(callExpr->funcName);
            if (it != functions.end()) {
                std::vector<llvm::Value*> args;
                llvm::Function* callee = it->second;
                size_t i = 0;
                for (auto& arg : callExpr->args) {
                    TypeKind argType = TypeKind::INT;
                    if (i < callee->arg_size()) {
                        llvm::Type* paramType = callee->getArg(i)->getType();
                        if (paramType->isDoubleTy()) {
                            argType = TypeKind::FLOAT;
                        } else if (paramType->isIntegerTy(64)) {
                            argType = TypeKind::LONG;
                        }
                    }
                    args.push_back(generateExpression(arg.get(), argType));
                    i++;
                }
                return builder.CreateCall(callee, args);
            }
            std::cerr << "Unknown function: " << callExpr->funcName << std::endl;
            exit(1);
        }

        if (auto* unaryExpr = dynamic_cast<UnaryExpr*>(expr)) {
            auto operand = generateExpression(unaryExpr->operand.get(), expectedType);
            if (unaryExpr->op == "!") {
                if (operand->getType()->isDoubleTy()) {
                    auto cmp = builder.CreateFCmpOEQ(operand, llvm::ConstantFP::get(builder.getDoubleTy(), 0.0));
                    return builder.CreateZExt(cmp, builder.getInt32Ty());
                } else if (operand->getType()->isIntegerTy(64)) {
                    auto cmp = builder.CreateICmpEQ(operand, builder.getInt64(0));
                    return builder.CreateZExt(cmp, builder.getInt32Ty());
                } else {
                    auto cmp = builder.CreateICmpEQ(operand, builder.getInt32(0));
                    return builder.CreateZExt(cmp, builder.getInt32Ty());
                }
            }
            if (unaryExpr->op == "-") {
                if (operand->getType()->isDoubleTy()) {
                    return builder.CreateFNeg(operand);
                } else {
                    return builder.CreateNeg(operand);
                }
            }
        }

        if (auto* binExpr = dynamic_cast<BinaryExpr*>(expr)) {
            if (binExpr->op == "&&") {
                llvm::Function* func = builder.GetInsertBlock()->getParent();
                llvm::BasicBlock* rhsBB = llvm::BasicBlock::Create(*context, "and.rhs", func);
                llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*context, "and.merge", func);
                
                auto lhs = generateExpression(binExpr->left.get(), TypeKind::INT);
                llvm::Value* lhsBool;
                if (lhs->getType()->isDoubleTy()) {
                    lhsBool = builder.CreateFCmpONE(lhs, llvm::ConstantFP::get(builder.getDoubleTy(), 0.0));
                } else if (lhs->getType()->isIntegerTy(64)) {
                    lhsBool = builder.CreateICmpNE(lhs, builder.getInt64(0));
                } else {
                    lhsBool = builder.CreateICmpNE(lhs, builder.getInt32(0));
                }
                
                llvm::BasicBlock* lhsBB = builder.GetInsertBlock();
                builder.CreateCondBr(lhsBool, rhsBB, mergeBB);
                
                builder.SetInsertPoint(rhsBB);
                auto rhs = generateExpression(binExpr->right.get(), TypeKind::INT);
                llvm::Value* rhsBool;
                if (rhs->getType()->isDoubleTy()) {
                    rhsBool = builder.CreateFCmpONE(rhs, llvm::ConstantFP::get(builder.getDoubleTy(), 0.0));
                } else if (rhs->getType()->isIntegerTy(64)) {
                    rhsBool = builder.CreateICmpNE(rhs, builder.getInt64(0));
                } else {
                    rhsBool = builder.CreateICmpNE(rhs, builder.getInt32(0));
                }
                rhsBB = builder.GetInsertBlock();
                builder.CreateBr(mergeBB);
                
                builder.SetInsertPoint(mergeBB);
                auto phi = builder.CreatePHI(builder.getInt1Ty(), 2);
                phi->addIncoming(builder.getFalse(), lhsBB);
                phi->addIncoming(rhsBool, rhsBB);
                return builder.CreateZExt(phi, builder.getInt32Ty());
            }

            if (binExpr->op == "||") {
                llvm::Function* func = builder.GetInsertBlock()->getParent();
                llvm::BasicBlock* rhsBB = llvm::BasicBlock::Create(*context, "or.rhs", func);
                llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(*context, "or.merge", func);
                
                auto lhs = generateExpression(binExpr->left.get(), TypeKind::INT);
                llvm::Value* lhsBool;
                if (lhs->getType()->isDoubleTy()) {
                    lhsBool = builder.CreateFCmpONE(lhs, llvm::ConstantFP::get(builder.getDoubleTy(), 0.0));
                } else if (lhs->getType()->isIntegerTy(64)) {
                    lhsBool = builder.CreateICmpNE(lhs, builder.getInt64(0));
                } else {
                    lhsBool = builder.CreateICmpNE(lhs, builder.getInt32(0));
                }
                
                llvm::BasicBlock* lhsBB = builder.GetInsertBlock();
                builder.CreateCondBr(lhsBool, mergeBB, rhsBB);
                
                builder.SetInsertPoint(rhsBB);
                auto rhs = generateExpression(binExpr->right.get(), TypeKind::INT);
                llvm::Value* rhsBool;
                if (rhs->getType()->isDoubleTy()) {
                    rhsBool = builder.CreateFCmpONE(rhs, llvm::ConstantFP::get(builder.getDoubleTy(), 0.0));
                } else if (rhs->getType()->isIntegerTy(64)) {
                    rhsBool = builder.CreateICmpNE(rhs, builder.getInt64(0));
                } else {
                    rhsBool = builder.CreateICmpNE(rhs, builder.getInt32(0));
                }
                rhsBB = builder.GetInsertBlock();
                builder.CreateBr(mergeBB);
                
                builder.SetInsertPoint(mergeBB);
                auto phi = builder.CreatePHI(builder.getInt1Ty(), 2);
                phi->addIncoming(builder.getTrue(), lhsBB);
                phi->addIncoming(rhsBool, rhsBB);
                return builder.CreateZExt(phi, builder.getInt32Ty());
            }

            auto left = generateExpression(binExpr->left.get(), expectedType);
            auto right = generateExpression(binExpr->right.get(), expectedType);

            bool isFloat = left->getType()->isDoubleTy() || right->getType()->isDoubleTy();
            bool isLong = !isFloat && (left->getType()->isIntegerTy(64) || right->getType()->isIntegerTy(64));
            
            if (isFloat) {
                if (!left->getType()->isDoubleTy()) {
                    if (left->getType()->isIntegerTy(64)) {
                        left = builder.CreateSIToFP(left, builder.getDoubleTy());
                    } else {
                        left = builder.CreateSIToFP(left, builder.getDoubleTy());
                    }
                }
                if (!right->getType()->isDoubleTy()) {
                    if (right->getType()->isIntegerTy(64)) {
                        right = builder.CreateSIToFP(right, builder.getDoubleTy());
                    } else {
                        right = builder.CreateSIToFP(right, builder.getDoubleTy());
                    }
                }
                binExpr->exprType = TypeKind::FLOAT;

                if (binExpr->op == "+") return builder.CreateFAdd(left, right);
                if (binExpr->op == "-") return builder.CreateFSub(left, right);
                if (binExpr->op == "*") return builder.CreateFMul(left, right);
                if (binExpr->op == "/") return builder.CreateFDiv(left, right);
                if (binExpr->op == "%") return builder.CreateFRem(left, right);
                if (binExpr->op == "<") return builder.CreateZExt(builder.CreateFCmpOLT(left, right), builder.getInt32Ty());
                if (binExpr->op == ">") return builder.CreateZExt(builder.CreateFCmpOGT(left, right), builder.getInt32Ty());
                if (binExpr->op == "<=") return builder.CreateZExt(builder.CreateFCmpOLE(left, right), builder.getInt32Ty());
                if (binExpr->op == ">=") return builder.CreateZExt(builder.CreateFCmpOGE(left, right), builder.getInt32Ty());
                if (binExpr->op == "==") return builder.CreateZExt(builder.CreateFCmpOEQ(left, right), builder.getInt32Ty());
                if (binExpr->op == "!=") return builder.CreateZExt(builder.CreateFCmpONE(left, right), builder.getInt32Ty());
            } else if (isLong) {
                if (!left->getType()->isIntegerTy(64)) {
                    left = builder.CreateSExt(left, builder.getInt64Ty());
                }
                if (!right->getType()->isIntegerTy(64)) {
                    right = builder.CreateSExt(right, builder.getInt64Ty());
                }
                binExpr->exprType = TypeKind::LONG;

                if (binExpr->op == "+") return builder.CreateAdd(left, right);
                if (binExpr->op == "-") return builder.CreateSub(left, right);
                if (binExpr->op == "*") return builder.CreateMul(left, right);
                if (binExpr->op == "/") return builder.CreateSDiv(left, right);
                if (binExpr->op == "%") return builder.CreateSRem(left, right);
                if (binExpr->op == "<") return builder.CreateZExt(builder.CreateICmpSLT(left, right), builder.getInt32Ty());
                if (binExpr->op == ">") return builder.CreateZExt(builder.CreateICmpSGT(left, right), builder.getInt32Ty());
                if (binExpr->op == "<=") return builder.CreateZExt(builder.CreateICmpSLE(left, right), builder.getInt32Ty());
                if (binExpr->op == ">=") return builder.CreateZExt(builder.CreateICmpSGE(left, right), builder.getInt32Ty());
                if (binExpr->op == "==") return builder.CreateZExt(builder.CreateICmpEQ(left, right), builder.getInt32Ty());
                if (binExpr->op == "!=") return builder.CreateZExt(builder.CreateICmpNE(left, right), builder.getInt32Ty());
            } else {
                binExpr->exprType = TypeKind::INT;

                if (binExpr->op == "+") return builder.CreateAdd(left, right);
                if (binExpr->op == "-") return builder.CreateSub(left, right);
                if (binExpr->op == "*") return builder.CreateMul(left, right);
                if (binExpr->op == "/") return builder.CreateSDiv(left, right);
                if (binExpr->op == "%") return builder.CreateSRem(left, right);
                if (binExpr->op == "<") return builder.CreateZExt(builder.CreateICmpSLT(left, right), builder.getInt32Ty());
                if (binExpr->op == ">") return builder.CreateZExt(builder.CreateICmpSGT(left, right), builder.getInt32Ty());
                if (binExpr->op == "<=") return builder.CreateZExt(builder.CreateICmpSLE(left, right), builder.getInt32Ty());
                if (binExpr->op == ">=") return builder.CreateZExt(builder.CreateICmpSGE(left, right), builder.getInt32Ty());
                if (binExpr->op == "==") return builder.CreateZExt(builder.CreateICmpEQ(left, right), builder.getInt32Ty());
                if (binExpr->op == "!=") return builder.CreateZExt(builder.CreateICmpNE(left, right), builder.getInt32Ty());
            }
        }

        return builder.getInt32(0);
    }
};

void printUsage(const char* programName) {
    std::cerr << "Usage:" << std::endl;
    std::cerr << "  " << programName << " <source.bs>           Run the program" << std::endl;
    std::cerr << "  " << programName << " -c <source.bs>        Compile to executable" << std::endl;
    std::cerr << "  " << programName << " -c <source.bs> -o <output>  Compile with custom output name" << std::endl;
    std::cerr << "  " << programName << " --emit-ir <source.bs> Print LLVM IR" << std::endl;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printUsage(argv[0]);
        return 1;
    }

    std::string sourceFile;
    std::string outputFile;
    bool compileMode = false;
    bool emitIR = false;

    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        if (arg == "-c") {
            compileMode = true;
        } else if (arg == "-o" && i + 1 < argc) {
            outputFile = argv[++i];
        } else if (arg == "--emit-ir") {
            emitIR = true;
        } else if (arg[0] != '-') {
            sourceFile = arg;
        }
    }

    if (sourceFile.empty()) {
        std::cerr << "Error: No source file specified" << std::endl;
        printUsage(argv[0]);
        return 1;
    }

    std::ifstream file(sourceFile, std::ios::binary);
    if (!file) {
        std::cerr << "Cannot open file: " << sourceFile << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    file.close();

    Lexer lexer(source);
    auto tokens = lexer.tokenize();

    Parser parser(tokens);
    auto program = parser.parse();

    CodeGenerator codegen;
    codegen.generate(program.get());

    if (emitIR) {
        codegen.printIR();
        return 0;
    }

    if (compileMode) {
        if (outputFile.empty()) {
            size_t lastDot = sourceFile.rfind('.');
            if (lastDot != std::string::npos) {
                outputFile = sourceFile.substr(0, lastDot);
            } else {
                outputFile = sourceFile + "_out";
            }
        }
        
        if (codegen.compileToExecutable(outputFile)) {
            std::cout << "Compiled successfully: " << outputFile;
#ifdef _WIN32
            std::cout << ".exe";
#endif
            std::cout << std::endl;
            return 0;
        } else {
            std::cerr << "Compilation failed" << std::endl;
            return 1;
        }
    }

    return codegen.runJIT();
}
