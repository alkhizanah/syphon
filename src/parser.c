#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <stdarg.h>
#include <stdbool.h>

#include "array.h"
#include "ast.h"
#include "lexer.h"
#include "parser.h"
#include "source_location.h"

typedef enum : uint8_t {
    PR_LOWEST,
    PR_ASSIGN,
    PR_COMPARISON,
    PR_SUM,
    PR_PRODUCT,
    PR_EXPONENT,
    PR_PREFIX,
    PR_POSTFIX,
} Precedence;

static Precedence precedence_of(TokenTag token) {
    switch (token) {
    case TOK_ASSIGN:
    case TOK_PLUS_ASSIGN:
    case TOK_MINUS_ASSIGN:
    case TOK_MULTIPLY_ASSIGN:
    case TOK_DIVIDE_ASSIGN:
    case TOK_EXPONENT_ASSIGN:
    case TOK_MODULO_ASSIGN:
        return PR_ASSIGN;

    case TOK_NOT_EQL:
    case TOK_EQL:
    case TOK_GREATER_THAN:
    case TOK_LESS_THAN:
    case TOK_GREATER_THAN_OR_EQL:
    case TOK_LESS_THAN_OR_EQL:
        return PR_COMPARISON;

    case TOK_PLUS:
    case TOK_MINUS:
        return PR_SUM;

    case TOK_MULTIPLY:
    case TOK_DIVIDE:
    case TOK_MODULO:
        return PR_PRODUCT;

    case TOK_EXPONENT:
        return PR_EXPONENT;

    case TOK_OPAREN:
    case TOK_OBRACKET:
    case TOK_DOT:
        return PR_POSTFIX;

    default:
        return PR_LOWEST;
    }
}

static inline Token parser_advance(Parser *parser) {
    parser->current_token = parser->next_token;
    parser->next_token = lexer_next(&parser->lexer);
    return parser->current_token;
}

static inline Token parser_peek(const Parser *parser) {
    return parser->next_token;
}

static AstNodeIdx parse_stmt(Parser *parser);

static AstNodeIdx parse_expr(Parser *parser, Precedence precedence);

[[gnu::format(printf, 3, 4)]]
static void parser_error(const Parser *parser, uint32_t start,
                         const char *format, ...) {
    va_list args;

    va_start(args, format);

    SourceLocation loc =
        source_location_of(parser->file_path, parser->lexer.buffer, start);

    fprintf(stderr, "%s:%u:%u: error: ", loc.file_path, loc.line, loc.column);
    vfprintf(stderr, format, args);

    va_end(args);
}

static AstNodeIdx parse_block(Parser *parser) {
    Token token = parser_advance(parser);

    AstExtra stmts = {0};

    while (parser_peek(parser).tag != TOK_CBRACE) {
        AstNodeIdx stmt = parse_stmt(parser);

        if (stmt == INVALID_NODE_IDX) {
            return INVALID_NODE_IDX;
        }

        if (parser_peek(parser).tag == TOK_EOF) {
            parser_error(parser, token.range.start, "'{' did not get closed\n");

            return INVALID_NODE_IDX;
        }

        ARRAY_PUSH(&stmts, stmt);
    }

    parser_advance(parser);

    uint32_t stmts_start = parser->ast.extra.count;

    ARRAY_EXPAND(&parser->ast.extra, stmts.items, stmts.count);

    AstNodeIdx block = ast_push(&parser->ast, NODE_BLOCK, stmts_start,
                                stmts.count, token.range.start);

    ARRAY_FREE(&stmts);

    return block;
}

static AstNodeIdx parse_assign(Parser *parser, AstNodeIdx target,
                               AstNodeTag tag) {
    Token token = parser_advance(parser);

    AstNodeIdx value = parse_expr(parser, PR_ASSIGN);

    if (value == INVALID_NODE_IDX) {
        return INVALID_NODE_IDX;
    }

    return ast_push(&parser->ast, tag, target, value, token.range.start);
}

static AstNodeIdx parse_call(Parser *parser, AstNodeIdx callee) {
    Token token = parser_advance(parser);

    AstExtra arguments = {0};

    while (parser_peek(parser).tag != TOK_CPAREN) {
        AstNodeIdx argument = parse_expr(parser, PR_LOWEST);

        if (argument == INVALID_NODE_IDX) {
            return INVALID_NODE_IDX;
        }

        if (parser_peek(parser).tag == TOK_COMMA) {
            parser_advance(parser);
        }

        if (parser_peek(parser).tag == TOK_EOF) {
            parser_error(parser, token.range.start, "'(' did not get closed\n");

            return INVALID_NODE_IDX;
        }

        ARRAY_PUSH(&arguments, argument);
    }

    parser_advance(parser);

    if (arguments.count == 0) {
        return ast_push(&parser->ast, NODE_CALL, callee, INVALID_EXTRA_IDX,
                        token.range.start);
    } else {
        uint32_t start = parser->ast.extra.count;

        ARRAY_PUSH(&parser->ast.extra, arguments.count);

        ARRAY_EXPAND(&parser->ast.extra, arguments.items, arguments.count);

        ARRAY_FREE(&arguments);

        return ast_push(&parser->ast, NODE_CALL, callee, start,
                        token.range.start);
    }
}

static AstNodeIdx parse_identifier(Parser *parser) {
    Token token = parser_advance(parser);

    return ast_push(&parser->ast, NODE_IDENTIFIER, token.range.start,
                    token.range.end, token.range.start);
}

static AstNodeIdx parse_member_access(Parser *parser, AstNodeIdx target) {
    Token token = parser_advance(parser);

    if (parser_peek(parser).tag != TOK_IDENTIFIER) {
        parser_error(parser, parser_peek(parser).range.start,
                     "expected an identifier after '.'\n");

        return INVALID_NODE_IDX;
    }

    AstNodeIdx identifier = parse_identifier(parser);

    return ast_push(&parser->ast, NODE_MEMBER, target, identifier,
                    token.range.start);
}

static AstNodeIdx parse_subscript_access(Parser *parser, AstNodeIdx target) {
    Token token = parser_advance(parser);

    AstNodeIdx node;

    if (parser_peek(parser).tag == TOK_COLON) {
        // a[:??
        parser_advance(parser);

        AstNodeIdx indices;

        if (parser_peek(parser).tag == TOK_CBRACKET) {
            // a[:]

            indices = ast_push(&parser->ast, 0, INVALID_NODE_IDX,
                               INVALID_NODE_IDX, token.range.start);
        } else {
            // a[:e]

            AstNodeIdx end = parse_expr(parser, PR_LOWEST);

            indices = ast_push(&parser->ast, 0, INVALID_NODE_IDX, end,
                               token.range.start);
        }

        node = ast_push(&parser->ast, NODE_SLICE, target, indices,
                        token.range.start);
    } else {
        AstNodeIdx start = parse_expr(parser, PR_LOWEST);

        if (parser_peek(parser).tag == TOK_COLON) {
            // a[s:??

            parser_advance(parser);

            AstNodeIdx indices;

            if (parser_peek(parser).tag == TOK_CBRACKET) {
                // a[s:]

                indices = ast_push(&parser->ast, 0, start, INVALID_NODE_IDX,
                                   token.range.start);
            } else {
                // a[s:e]

                AstNodeIdx end = parse_expr(parser, PR_LOWEST);

                indices =
                    ast_push(&parser->ast, 0, start, end, token.range.start);
            }

            node = ast_push(&parser->ast, NODE_SLICE, target, indices,
                            token.range.start);
        } else {
            // a[i]

            node = ast_push(&parser->ast, NODE_SUBSCRIPT, target, start,
                            token.range.start);
        }
    }

    if (parser_peek(parser).tag != TOK_CBRACKET) {
        parser_error(parser, token.range.start, "'[' did not get closed\n");

        return INVALID_NODE_IDX;
    }

    parser_advance(parser);

    return node;
}

static AstNodeIdx parse_binary_op(Parser *parser, AstNodeIdx lhs,
                                  AstNodeTag tag, Precedence precedence) {
    Token token = parser_advance(parser);

    AstNodeIdx rhs = parse_expr(parser, precedence);

    if (rhs == INVALID_NODE_IDX) {
        return INVALID_NODE_IDX;
    }

    return ast_push(&parser->ast, tag, lhs, rhs, token.range.start);
}

static AstNodeIdx parse_binary_expr(Parser *parser, AstNodeIdx lhs) {
    switch (parser_peek(parser).tag) {
    case TOK_ASSIGN:
        return parse_assign(parser, lhs, NODE_ASSIGN);
    case TOK_PLUS_ASSIGN:
        return parse_assign(parser, lhs, NODE_ASSIGN_ADD);
    case TOK_MINUS_ASSIGN:
        return parse_assign(parser, lhs, NODE_ASSIGN_SUB);
    case TOK_MULTIPLY_ASSIGN:
        return parse_assign(parser, lhs, NODE_ASSIGN_MUL);
    case TOK_DIVIDE_ASSIGN:
        return parse_assign(parser, lhs, NODE_ASSIGN_DIV);
    case TOK_EXPONENT_ASSIGN:
        return parse_assign(parser, lhs, NODE_ASSIGN_POW);
    case TOK_MODULO_ASSIGN:
        return parse_assign(parser, lhs, NODE_ASSIGN_MOD);
    case TOK_OPAREN:
        return parse_call(parser, lhs);
    case TOK_DOT:
        return parse_member_access(parser, lhs);
    case TOK_OBRACKET:
        return parse_subscript_access(parser, lhs);
    case TOK_PLUS:
        return parse_binary_op(parser, lhs, NODE_ADD, PR_SUM);
    case TOK_MINUS:
        return parse_binary_op(parser, lhs, NODE_SUB, PR_SUM);
    case TOK_MULTIPLY:
        return parse_binary_op(parser, lhs, NODE_MUL, PR_PRODUCT);
    case TOK_DIVIDE:
        return parse_binary_op(parser, lhs, NODE_DIV, PR_PRODUCT);
    case TOK_EXPONENT:
        return parse_binary_op(parser, lhs, NODE_POW, PR_EXPONENT);
    case TOK_MODULO:
        return parse_binary_op(parser, lhs, NODE_MOD, PR_PRODUCT);
    case TOK_EQL:
        return parse_binary_op(parser, lhs, NODE_EQL, PR_COMPARISON);
    case TOK_NOT_EQL:
        return parse_binary_op(parser, lhs, NODE_NEQ, PR_COMPARISON);
    case TOK_LESS_THAN:
        return parse_binary_op(parser, lhs, NODE_LT, PR_COMPARISON);
    case TOK_LESS_THAN_OR_EQL:
        return parse_binary_op(parser, lhs, NODE_LTE, PR_COMPARISON);
    case TOK_GREATER_THAN:
        return parse_binary_op(parser, lhs, NODE_GT, PR_COMPARISON);
    case TOK_GREATER_THAN_OR_EQL:
        return parse_binary_op(parser, lhs, NODE_GTE, PR_COMPARISON);
    default:
        parser_error(parser, parser_peek(parser).range.start,
                     "unknown binary operator\n");

        return INVALID_NODE_IDX;
    }
}

static AstNodeIdx parse_string(Parser *parser) {
    Token token = parser_advance(parser);

    bool unescaping = false;

    uint32_t unescaped_string_start = parser->ast.strings.count;

    size_t count = token.range.end - token.range.start;

    for (size_t i = 0; i < count; i++) {
        const char escaped = parser->lexer.buffer[token.range.start + i];

        char unescaped;

        if (unescaping) {
            switch (escaped) {
            case '\\':
                unescaped = '\\';
                break;
            case 'n':
                unescaped = '\n';
                break;
            case 'r':
                unescaped = '\r';
                break;
            case 't':
                unescaped = '\t';
                break;
            case 'e':
                unescaped = 27;
                break;
            case 'v':
                unescaped = 11;
                break;
            case 'b':
                unescaped = 8;
                break;
            case 'f':
                unescaped = 12;
                break;
            case '"':
                unescaped = '"';
                break;
            default:
                parser_error(parser, token.range.start + i,
                             "invalid string escape character: '%c'\n",
                             escaped);

                return INVALID_NODE_IDX;
            }

            unescaping = false;
        } else if (escaped == '\\') {
            unescaping = true;
            continue;
        } else {
            unescaped = escaped;
        }

        ARRAY_PUSH(&parser->ast.strings, unescaped);
    }

    uint32_t unescaped_string_end = parser->ast.strings.count;

    return ast_push(&parser->ast, NODE_STRING, unescaped_string_start,
                    unescaped_string_end - unescaped_string_start,
                    token.range.start);
}

static AstNodeIdx parse_int(Parser *parser) {
    Token token = parser_advance(parser);

    char *endptr;

    errno = 0;

    uint64_t v = strtoull(parser->lexer.buffer + token.range.start, &endptr, 0);

    if (errno == ERANGE) {
        parser_error(parser, token.range.start,
                     "number too big to be represented\n");

        return INVALID_NODE_IDX;
    }

    if (endptr != parser->lexer.buffer + token.range.end) {
        parser_error(parser, token.range.start,
                     "unsuitable digit in number: '%c'\n", *endptr);

        return INVALID_NODE_IDX;
    }

    return ast_push(&parser->ast, NODE_INT, v >> 32, v, token.range.start);
}

static AstNodeIdx parse_float(Parser *parser) {
    Token token = parser_advance(parser);

    char *endptr;

    errno = 0;

    double v = strtod(parser->lexer.buffer + token.range.start, &endptr);

    if (errno == ERANGE) {
        parser_error(parser, token.range.start,
                     "number too big to be represented\n");

        return INVALID_NODE_IDX;
    }

    if (endptr != parser->lexer.buffer + token.range.end) {
        parser_error(parser, token.range.start,
                     "unsuitable digit in number: '%c'\n", *endptr);

        return INVALID_NODE_IDX;
    }

    uint64_t vi;

    memcpy(&vi, &v, sizeof(double));

    return ast_push(&parser->ast, NODE_FLOAT, vi >> 32, vi, token.range.start);
}

static AstNodeIdx parse_parentheses_expr(Parser *parser) {
    Token token = parser_advance(parser);

    AstNodeIdx value = parse_expr(parser, PR_LOWEST);

    if (value == INVALID_NODE_IDX) {
        return INVALID_NODE_IDX;
    }

    if (parser_peek(parser).tag != TOK_CPAREN) {
        parser_error(parser, token.range.start, "'(' did not get closed\n");

        return INVALID_NODE_IDX;
    }

    parser_advance(parser);

    return value;
}

static AstNodeIdx parse_function(Parser *parser) {
    Token fn_token = parser_advance(parser);

    AstExtra parameters = {0};

    while (parser_peek(parser).tag != TOK_RARROW &&
           parser_peek(parser).tag != TOK_OBRACE) {
        if (parser_peek(parser).tag != TOK_IDENTIFIER) {
            parser_error(parser, parser_peek(parser).range.start,
                         "expected a parameter name to be an identifier\n");

            return INVALID_NODE_IDX;
        }

        AstNodeIdx parameter = parse_identifier(parser);

        if (parameter == INVALID_NODE_IDX) {
            return INVALID_NODE_IDX;
        }

        if (parser_peek(parser).tag == TOK_COMMA) {
            parser_advance(parser);
        }

        if (parser_peek(parser).tag == TOK_EOF) {
            parser_error(parser, parser_peek(parser).range.start,
                         "expected `=>` or `{` got end of file\n");

            return INVALID_NODE_IDX;
        }

        ARRAY_PUSH(&parameters, parameter);
    }

    AstNodeIdx block;

    if (parser_peek(parser).tag == TOK_RARROW) {
        Token arrow = parser_advance(parser);

        AstNodeIdx value = parse_expr(parser, PR_LOWEST);

        if (value == INVALID_NODE_IDX) {
            return INVALID_NODE_IDX;
        }

        block =
            ast_push(&parser->ast, NODE_RETURN, 0, value, arrow.range.start);
    } else if (parser_peek(parser).tag == TOK_OBRACE) {
        block = parse_block(parser);

        if (block == INVALID_NODE_IDX) {
            return INVALID_NODE_IDX;
        }
    }

    if (parameters.count == 0) {
        return ast_push(&parser->ast, NODE_FUNCTION, INVALID_EXTRA_IDX, block,
                        fn_token.range.start);
    } else {
        uint32_t start = parser->ast.extra.count;

        ARRAY_PUSH(&parser->ast.extra, parameters.count);

        ARRAY_EXPAND(&parser->ast.extra, parameters.items, parameters.count);

        ARRAY_FREE(&parameters);

        return ast_push(&parser->ast, NODE_FUNCTION, start, block,
                        fn_token.range.start);
    }
}

static AstNodeIdx parse_array(Parser *parser) {
    Token token = parser_advance(parser);

    AstExtra values = {0};

    while (parser_peek(parser).tag != TOK_CBRACKET) {
        AstNodeIdx value = parse_expr(parser, PR_LOWEST);

        if (value == INVALID_NODE_IDX) {
            return INVALID_NODE_IDX;
        }

        if (parser_peek(parser).tag == TOK_COMMA) {
            parser_advance(parser);
        }

        if (parser_peek(parser).tag == TOK_EOF) {
            parser_error(parser, token.range.start, "'[' did not get closed\n");

            return INVALID_NODE_IDX;
        }

        ARRAY_PUSH(&values, value);
    }

    parser_advance(parser);

    uint32_t start = parser->ast.extra.count;

    ARRAY_EXPAND(&parser->ast.extra, values.items, values.count);

    uint32_t count = values.count;

    ARRAY_FREE(&values);

    return ast_push(&parser->ast, NODE_ARRAY, start, count, token.range.start);
}

static AstNodeIdx parse_map(Parser *parser) {
    Token token = parser_advance(parser);

    AstExtra pairs = {0};

    while (parser_peek(parser).tag != TOK_CBRACE) {
        AstNodeIdx key = parse_expr(parser, PR_LOWEST);

        if (key == INVALID_NODE_IDX) {
            return INVALID_NODE_IDX;
        }

        if (parser_peek(parser).tag != TOK_COLON) {
            parser_error(parser, parser_peek(parser).range.start,
                         "expected ':'\n");

            return INVALID_NODE_IDX;
        }

        parser_advance(parser);

        AstNodeIdx value = parse_expr(parser, PR_LOWEST);

        if (value == INVALID_NODE_IDX) {
            return INVALID_NODE_IDX;
        }

        if (parser_peek(parser).tag == TOK_COMMA) {
            parser_advance(parser);
        }

        if (parser_peek(parser).tag == TOK_EOF) {
            parser_error(parser, token.range.start, "'{' did not get closed\n");

            return INVALID_NODE_IDX;
        }

        ARRAY_PUSH(&pairs, key);
        ARRAY_PUSH(&pairs, value);
    }

    parser_advance(parser);

    uint32_t pairs_start = parser->ast.extra.count;

    ARRAY_EXPAND(&parser->ast.extra, pairs.items, pairs.count);

    uint32_t pairs_count = pairs.count / 2;

    ARRAY_FREE(&pairs);

    return ast_push(&parser->ast, NODE_MAP, pairs_start, pairs_count,
                    token.range.start);
}

static AstNodeIdx parse_unary_op(Parser *parser, AstNodeTag tag) {
    Token token = parser_advance(parser);

    AstNodeIdx value = parse_expr(parser, PR_PREFIX);

    if (value == INVALID_NODE_IDX) {
        return INVALID_NODE_IDX;
    }

    return ast_push(&parser->ast, tag, 0, value, token.range.start);
}

static AstNodeIdx parse_unary_expr(Parser *parser) {
    switch (parser_peek(parser).tag) {
    case TOK_IDENTIFIER:
        return parse_identifier(parser);
    case TOK_STRING:
        return parse_string(parser);
    case TOK_INT:
        return parse_int(parser);
    case TOK_FLOAT:
        return parse_float(parser);
    case TOK_OPAREN:
        return parse_parentheses_expr(parser);
    case TOK_KEYWORD_FN:
        return parse_function(parser);
    case TOK_OBRACKET:
        return parse_array(parser);
    case TOK_OBRACE:
        return parse_map(parser);
    case TOK_MINUS:
        return parse_unary_op(parser, NODE_NEG);
    case TOK_LOGICAL_NOT:
        return parse_unary_op(parser, NODE_NOT);
    default:
        parser_error(parser, parser_peek(parser).range.start,
                     "unknown expression\n");

        return INVALID_NODE_IDX;
    }
}

static AstNodeIdx parse_expr(Parser *parser, Precedence precedence) {
    AstNodeIdx lhs = parse_unary_expr(parser);

    while (precedence_of(parser_peek(parser).tag) > precedence) {
        if (lhs == INVALID_NODE_IDX) {
            return INVALID_NODE_IDX;
        }

        lhs = parse_binary_expr(parser, lhs);
    }

    return lhs;
}

static AstNodeIdx parse_while_loop(Parser *parser) {
    Token token = parser_advance(parser);

    AstNodeIdx condition = parse_expr(parser, PR_LOWEST);

    if (condition == INVALID_NODE_IDX) {
        return INVALID_NODE_IDX;
    }

    AstNodeIdx block;

    if (parser_peek(parser).tag == TOK_RARROW) {
        parser_advance(parser);

        block = parse_stmt(parser);
    } else if (parser_peek(parser).tag == TOK_OBRACE) {
        block = parse_block(parser);
    } else {
        parser_error(parser, parser_peek(parser).range.start,
                     "expected '=>' or '{'\n");

        return INVALID_NODE_IDX;
    }

    if (block == INVALID_NODE_IDX) {
        return INVALID_NODE_IDX;
    }

    return ast_push(&parser->ast, NODE_WHILE,

                    condition, block, token.range.start);
}

static AstNodeIdx parse_conditional(Parser *parser) {
    Token token = parser_advance(parser);

    AstNodeIdx condition = parse_expr(parser, PR_LOWEST);

    if (condition == INVALID_NODE_IDX) {
        return INVALID_NODE_IDX;
    }

    AstNodeIdx true_case;

    if (parser_peek(parser).tag == TOK_RARROW) {
        parser_advance(parser);

        true_case = parse_stmt(parser);
    } else if (parser_peek(parser).tag == TOK_OBRACE) {
        true_case = parse_block(parser);
    } else {
        parser_error(parser, parser_peek(parser).range.start,
                     "expected '=>' or '{'\n");

        return INVALID_NODE_IDX;
    }

    if (true_case == INVALID_NODE_IDX) {
        return INVALID_NODE_IDX;
    }

    AstNodeIdx false_case = INVALID_NODE_IDX;

    if (parser_peek(parser).tag == TOK_KEYWORD_ELSE) {
        parser_advance(parser);

        switch (parser_peek(parser).tag) {
        case TOK_KEYWORD_IF:
            false_case = parse_conditional(parser);

            if (false_case == INVALID_NODE_IDX) {
                return INVALID_NODE_IDX;
            }

            break;

        case TOK_RARROW:
            parser_advance(parser);

            false_case = parse_stmt(parser);

            if (false_case == INVALID_NODE_IDX) {
                return INVALID_NODE_IDX;
            }

            break;

        case TOK_OBRACE:
            false_case = parse_block(parser);

            if (false_case == INVALID_NODE_IDX) {
                return INVALID_NODE_IDX;
            }

            break;

        default: {
            parser_error(parser, parser_peek(parser).range.start,
                         "expected '=>' or '{' or 'if'\n");

            return INVALID_NODE_IDX;
        }
        }
    }

    uint32_t rhs = parser->ast.extra.count;

    ARRAY_PUSH(&parser->ast.extra, true_case);
    ARRAY_PUSH(&parser->ast.extra, false_case);

    return ast_push(&parser->ast, NODE_IF, condition, rhs, token.range.start);
}

static AstNodeIdx parse_return(Parser *parser) {
    Token token = parser_advance(parser);

    AstNodeIdx value = parse_expr(parser, PR_LOWEST);

    if (value == INVALID_NODE_IDX) {
        return INVALID_NODE_IDX;
    }

    return ast_push(&parser->ast, NODE_RETURN, 0, value, token.range.start);
}

static AstNodeIdx parse_stmt(Parser *parser) {
    switch (parser_peek(parser).tag) {
    case TOK_OBRACE:
        return parse_block(parser);
    case TOK_KEYWORD_WHILE:
        return parse_while_loop(parser);
    case TOK_KEYWORD_IF:
        return parse_conditional(parser);
    case TOK_KEYWORD_RETURN:
        return parse_return(parser);
    case TOK_KEYWORD_BREAK:
        return ast_push(&parser->ast, NODE_BREAK, 0, 0,
                        parser_advance(parser).range.start);
    case TOK_KEYWORD_CONTINUE:
        return ast_push(&parser->ast, NODE_CONTINUE, 0, 0,
                        parser_advance(parser).range.start);
    default:
        return parse_expr(parser, PR_LOWEST);
    }
}

AstNodeIdx parse(Parser *parser) {
    AstExtra stmts = {0};

    parser_advance(parser);

    while (parser_peek(parser).tag != TOK_EOF) {
        AstNodeIdx stmt = parse_stmt(parser);

        if (stmt == INVALID_NODE_IDX) {
            return INVALID_NODE_IDX;
        }

        ARRAY_PUSH(&stmts, stmt);
    }

    uint32_t stmts_start = parser->ast.extra.count;

    ARRAY_EXPAND(&parser->ast.extra, stmts.items, stmts.count);

    AstNodeIdx program =
        ast_push(&parser->ast, NODE_BLOCK, stmts_start, stmts.count, 0);

    ARRAY_FREE(&stmts);

    return program;
}
