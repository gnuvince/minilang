# -*- coding: utf-8 -*-

import sys

# Token types
TOK_PRINT  = 0
TOK_ID     = 1
TOK_VAR    = 2
TOK_INT    = 3
TOK_FLOAT  = 4
TOK_TYPE   = 5
TOK_EQ     = 6
TOK_PLUS   = 7
TOK_STAR   = 8
TOK_LPAREN = 9
TOK_RPAREN = 10
TOK_COLON  = 11

# AST node categories (we use the word category to not be confused
# with the word type)
AST_DECL   = 0
AST_ASSIGN = 1
AST_PRINT  = 2
AST_INT    = 3
AST_FLOAT  = 4
AST_ID     = 5
AST_ADD    = 6
AST_MUL    = 7


def error(msg):
    print("Error: " + msg)
    sys.exit(1)

def tok(ty, val):
    return { "type": ty, "value": val }

def node(cat, **args):
    return dict(cat=cat, **args)

def lex(s):
    """
    Input : a string representing a mini program
    Output: a list of tokens

    lex(s) will produce a sequence of tokens, which are dicts with two
    bindings: the type of the token (as defined above) and a semantic
    value.  The semantic value is a piece of information associated
    with the token, such as the name of an identifier or the value of
    an integer literal.  Some tokens, like the plus symbol, do not
    have an associated semantic value.
    """
    i = 0
    tokens = []
    while i < len(s):
        c = s[i]

        # Skip spaces
        if c.isspace():
            i += 1
            continue

        # Operators and punctuation
        elif c == "=":
            tokens.append(tok(TOK_EQ, None))
        elif c == "+":
            tokens.append(tok(TOK_PLUS, None))
        elif c == "*":
            tokens.append(tok(TOK_STAR, None))
        elif c == "(":
            tokens.append(tok(TOK_LPAREN, None))
        elif c == ")":
            tokens.append(tok(TOK_RPAREN, None))
        elif c == ":":
            tokens.append(tok(TOK_COLON, None))

        # Integer and float literals
        elif c.isdigit():
            num = ""
            while s[i].isdigit():
                num += s[i]
                i += 1
            if s[i] == ".":
                num += "."
                i += 1
                while s[i].isdigit():
                    num += s[i]
                    i += 1
                i -= 1
                tokens.append(tok(TOK_FLOAT, float(num)))
            else:
                i -= 1
                tokens.append(tok(TOK_INT, int(num)))

        # Identifiers and keywords
        elif c.isalpha() or c == "_":
            ident = ""
            while s[i].isalnum() or s[i] == "_":
                ident += s[i]
                i += 1
            i -= 1 # Read one char too many
            if ident == "print":
                tokens.append(tok(TOK_PRINT, None))
            elif ident == "var":
                tokens.append(tok(TOK_VAR, None))
            elif ident in ("int", "float"):
                tokens.append(tok(TOK_TYPE, ident))
            else:
                tokens.append(tok(TOK_ID, ident))
        else:
            error("invalid character: %r" % c)
        i += 1
    return tokens


def parse(toks):
    """
    Input : a list of tokens
    Output: a list of statement nodes

    We parse the tokens according to the following grammar.  Every
    non-terminal (left-hand side of a ::=) has its own local function
    definition.

        program  ::=  decls stmts
        decls    ::=  decl decls
                   |  ε
        decl     ::=  ident ':' type
        stmts    ::=  stmt stmts
                   |  ε
        stmt     ::=  ident '=' expr
                   |  'print' expr
        expr     ::=  term '+' expr
                   |  term
        term     ::=  factor '*' term
                   |  factor
        factor   ::=  '(' expr ')'
                   |  ident
                   |  int

    The AST nodes are represented with dicts as follows:
    - Declarations
        - var id: type   : { "cat": AST_DECL, id: "x", "type": type }

    - Statements
        - id = expr      : { "cat": AST_ASSIGN, "lhs": id, "rhs": expr }
        - print expr     : { "cat": AST_PRINT, "expr": expr }

    - Expressions
        - int            : { "cat": AST_INT, "value": int }
        - float          : { "cat": AST_FLOAT, "value": float }
        - id             : { "cat": AST_ID, "name": id }
        - e1 + e2        : { "cat": AST_ADD, "lhs": e1, "rhs": e2 }
        - e1 * e2        : { "cat": AST_MUL, "lhs": e1, "rhs": e2 }

    For example, here is a simple statement and its AST representation:

        x = 3 + y

        {
          "cat": AST_ASSIGN,
          "lhs": "x",
          "expr": {
            "cat": AST_ADD,
            "lhs": { "cat": AST_INT, "value": 3 },
            "rhs": { "cat": AST_ID, "id": "y" }
          }
        }
    """

    def consume(tok_type):
        if tok_type == toks[0]["type"]:
            t = toks.pop(0)
            return t
        else:
            error("expected %d, found %d" % (tok_type, toks[0]["type"]))

    def peek():
        if toks:
            return toks[0]["type"]
        else:
            return None

    def program():
        ds = decls()
        sts = stmts()
        return {
            "decls": ds,
            "stmts": sts,
        }

    def decls():
        ds = []
        while peek() == TOK_VAR:
            ds.append(decl())
        return ds

    def decl():
        if peek() == TOK_VAR:
            consume(TOK_VAR)
            id = consume(TOK_ID)
            consume(TOK_COLON)
            ty = consume(TOK_TYPE)
            return node(AST_DECL, id=id["value"], type=ty["value"])
        else:
            error("not a valid declaration")

    def stmts():
        ss = []
        while peek() in (TOK_PRINT, TOK_ID):
            ss.append(stmt())
        return ss

    def stmt():
        next_tok = peek()
        if next_tok == TOK_ID:
            id = consume(TOK_ID)
            consume(TOK_EQ)
            e = expr()
            return node(AST_ASSIGN, lhs=id["value"], rhs=e)
        elif next_tok == TOK_PRINT:
            consume(TOK_PRINT)
            e = expr()
            return node(AST_PRINT, expr=e)
        else:
            error("illegal statement")

    def expr():
        t = term()
        next_tok = peek()
        if next_tok == TOK_PLUS:
            consume(TOK_PLUS)
            e = expr()
            return node(AST_ADD, lhs=t, rhs=e)
        return t

    def term():
        f = factor()
        next_tok = peek()
        if next_tok == TOK_STAR:
            consume(TOK_STAR)
            t = term()
            return node(AST_MUL, lhs=f, rhs=t)
        return f

    def factor():
        next_tok = peek()
        if next_tok == TOK_LPAREN:
            consume(TOK_LPAREN)
            e = expr()
            consume(TOK_RPAREN)
            return e
        elif next_tok == TOK_INT:
            tok = consume(TOK_INT)
            return node(AST_INT, value=tok["value"])
        elif next_tok == TOK_FLOAT:
            tok = consume(TOK_FLOAT)
            return node(AST_FLOAT, value=tok["value"])
        elif next_tok == TOK_ID:
            tok = consume(TOK_ID)
            return node(AST_ID, name=tok["value"])
        else:
            error("illegal token %d" % next_tok)

    return program()


def build_symtab(ast):
    """
    Input : the AST of a mini program
    Output: a dictionary mapping variable names to types

    This procedure takes iterates over the declarations and adds them
    to a symbol table.  If a variable is declared more than once, we
    report an error.
    """
    symtab = {}
    for decl in ast["decls"]:
        if decl["id"] in symtab:
            error("%s is already declared" % decl["id"])
        else:
            symtab[decl["id"]] = decl["type"]
    return symtab


def typecheck(ast, symtab):
    """
    Input : the AST of a mini program and its associated symbol table
    Output: an AST of the mini program, but with extra type
    information added inside expression node

    The typing rules of our small language are pretty simple:

    - We have two types, int and float
    - An int literal has type int
    - A float literal has type float
    - There is no automatic conversion from int to float (in fact, the
      language does not support conversions)
    - The two operands of an arithmetic operations must be of the same type
    - An expression can be assigned to a variable only if their types are equal
    """
    def check_stmt(stmt):
        if stmt["cat"] == AST_PRINT:
            typed_expr = check_expr(stmt["expr"])
            return node(AST_PRINT, expr=typed_expr)
        elif stmt["cat"] == AST_ASSIGN:
            typed_rhs = check_expr(stmt["rhs"])
            if typed_rhs["type"] == symtab[stmt["lhs"]]:
                return node(AST_ASSIGN, lhs=stmt["lhs"], rhs=typed_rhs)
            else:
                error("expected %s, got %s" % (symtab[stmt["lhs"]], typed_rhs["type"]))

    def check_expr(expr):
        if expr["cat"] == AST_INT:
            return node(AST_INT, value=expr["value"], type="int")
        elif expr["cat"] == AST_FLOAT:
            return node(AST_FLOAT, value=expr["value"], type="float")
        elif expr["cat"] == AST_ID:
            return node(AST_ID, name=expr["name"], type=symtab[expr["name"]])
        elif expr["cat"] in (AST_ADD, AST_MUL):
            typed_e1 = check_expr(expr["lhs"])
            typed_e2 = check_expr(expr["rhs"])
            if typed_e1["type"] == typed_e2["type"]:
                return node(expr["cat"], lhs=typed_e1, rhs=typed_e2, type=typed_e1["type"])
            else:
                error("operands must have the same type")

    updated_stmts = []
    for stmt in ast["stmts"]:
        updated_stmts.append(check_stmt(stmt))
    return { "decls": ast["decls"], "stmts": updated_stmts }


curr_tmp = 0
def codegen(ast, symtab):
    """
    Input : the AST and symbol table of a mini program
    Output: an equivalent C program

    codegen(ast) will generate code in three-address code.  The code
    is clearly not optimal, nor even really human readable, however it
    is (a) correct, and (b) translated easily.

    For expressions, we give the expression to translate, and the name
    of the temporary in which the result should be put.  This result
    will be used by the parent of the expression.  The new_temp() function
    creates a new temporary variable for every time it's called.
    """
    def new_temp():
        global curr_tmp
        curr_tmp += 1
        return "t_" + str(curr_tmp)

    def gen_stmt(stmt):
        if stmt["cat"] == AST_ASSIGN:
            if stmt["lhs"] not in symtab:
                error("undeclared variable: %s" % stmt["lhs"])
            tmp = new_temp()
            expr_out = gen_expr(stmt["rhs"], tmp)
            return expr_out + ["%s = %s;" % (stmt["lhs"], tmp)]
        elif stmt["cat"] == AST_PRINT:
            tmp = new_temp()
            expr_out = gen_expr(stmt["expr"], tmp)
            if stmt["expr"]["type"] == "int":
                flag = "d"
            else:
                flag = "f"
            return expr_out + ['printf("%%%s\\n", %s);' % (flag, tmp)]

    def gen_expr(expr, result_register):
        if expr["cat"] in (AST_INT, AST_FLOAT):
            return ["%s %s = %s;" % (expr["type"], result_register, expr["value"])]
        elif expr["cat"] == AST_ID:
            if expr["name"] not in symtab:
                error("undeclared variable: %s" % expr["name"])
            return ["%s %s = %s;" % (expr["type"], result_register, expr["name"])]
        elif expr["cat"] in (AST_ADD, AST_MUL):
            t1 = new_temp()
            e1 = gen_expr(expr["lhs"], t1)
            t2 = new_temp()
            e2 = gen_expr(expr["rhs"], t2)
            if expr["cat"] == AST_ADD:
                op = "+"
            else:
                op = "*"
            return e1 + e2 + ["%s %s = %s %s %s;" % (expr["type"], result_register, t1, op, t2)]

    output = []

    # Add the usual C headers and main declaration.
    output += ["#include <stdio.h>"]
    output += ["int main(void) {"]

    # Add the variable declarations
    for var, ty in symtab.iteritems():
        output += ["%s %s;" % (ty, var)]

    # Add the C statements to the main function.
    for stmt in ast["stmts"]:
        output += gen_stmt(stmt)

    output += ["}"]
    return output




def main():
    src = sys.stdin.read()
    toks = lex(src)                      # source -> tokens
    ast = parse(toks)                    # tokens -> AST
    symtab = build_symtab(ast)           # AST -> symbol table
    typed_ast = typecheck(ast, symtab)   # AST -> Typed AST
    for s in codegen(typed_ast, symtab): # (Typed AST * symbol table) -> C code
        print(s)

if __name__ == "__main__":
    main()
