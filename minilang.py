# -*- coding: utf-8 -*-

# Copyright (c) 2014-2015 Vincent Foley-Bourgon

# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:

# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


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
TOK_MINUS  = 8
TOK_STAR   = 9
TOK_SLASH  = 10
TOK_LPAREN = 11
TOK_RPAREN = 12
TOK_COLON  = 13
TOK_WHILE  = 14
TOK_DO     = 15
TOK_DONE   = 16
TOK_SEMI   = 17
TOK_READ   = 18

# AST nodes
AST_DECL   = 0
AST_ASSIGN = 1
AST_PRINT  = 2
AST_INT    = 3
AST_FLOAT  = 4
AST_ID     = 5
AST_BINOP  = 6
AST_WHILE  = 7
AST_READ   = 8


def error(msg):
    print("Error: " + msg)
    sys.exit(1)

def tok(ty, val):
    return { "toktype": ty, "value": val }

def astnode(nodetype, **args):
    return dict(nodetype=nodetype, **args)

def lex(s):
    """
    Input : a string representing a mini program
    Output: a list of tokens

    lex(s) will produce a sequence of tokens, which are dicts with two
    bindings: the type of the token (as defined above) and a semantic
    value.  The semantic value (also called lexeme) is a piece of
    information associated with the token, such as the name of an
    identifier or the value of an integer literal.  Some tokens, like
    the plus symbol, do not have an associated semantic value.

    Example:
    x = x + dx;
    =>
    { "toktype": TOK_ID   , "value": "x" }
    { "toktype": TOK_EQ   , "value": None }
    { "toktype": TOK_ID   , "value": "x" }
    { "toktype": TOK_PLUS , "value": None }
    { "toktype": TOK_ID   , "value": "dx" }
    { "toktype": TOK_SEMI , "value": None }

    alpha   ::= ['a'-'z'  'A'-'Z'  '_']
    digit   ::= ['0'-'9']
    alnum   ::= alpha | digit
    int     ::= digit+
    float   ::= digit+ '.' digit*
    keyword ::= "var" | "print" | "read" | "while" | "do" | "done" | "int" | "float"
    ident   ::= alpha alnum*
    """
    i = 0
    tokens = []
    while i < len(s):
        c = s[i]

        # Skip spaces
        if c.isspace():
            pass

        # Skip comments
        elif c == "#":
            while s[i] != "\n":
                i += 1

        # Operators and punctuation
        elif c == "=":
            tokens.append(tok(TOK_EQ, None))
        elif c == "+":
            tokens.append(tok(TOK_PLUS, None))
        elif c == "-":
            tokens.append(tok(TOK_MINUS, None))
        elif c == "*":
            tokens.append(tok(TOK_STAR, None))
        elif c == "/":
            tokens.append(tok(TOK_SLASH, None))
        elif c == "(":
            tokens.append(tok(TOK_LPAREN, None))
        elif c == ")":
            tokens.append(tok(TOK_RPAREN, None))
        elif c == ":":
            tokens.append(tok(TOK_COLON, None))
        elif c == ";":
            tokens.append(tok(TOK_SEMI, None))

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
                tokens.append(tok(TOK_FLOAT, float(num)))
            else:
                tokens.append(tok(TOK_INT, int(num)))
            i -= 1 # Read one char too many, readjust.

        # Identifiers and keywords
        elif c.isalpha() or c == "_":
            ident = ""
            while s[i].isalnum() or s[i] == "_":
                ident += s[i]
                i += 1
            i -= 1 # Read one char too many, readjust.
            if ident == "print":
                tokens.append(tok(TOK_PRINT, None))
            elif ident == "read":
                tokens.append(tok(TOK_READ, None))
            elif ident == "var":
                tokens.append(tok(TOK_VAR, None))
            elif ident == "while":
                tokens.append(tok(TOK_WHILE, None))
            elif ident == "do":
                tokens.append(tok(TOK_DO, None))
            elif ident == "done":
                tokens.append(tok(TOK_DONE, None))
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

    parse(toks) is a predictive, recursive-descent parser that will
    return a list of AST nodes (declarations and statements) from the
    token stream computer by lex() above.  We parse the tokens
    according to the following grammar.  Every non-terminal (left-hand
    side of a ::=) has its own local function definition.

        program  ::=  decls stmts
        decls    ::=  decl decls
                   |  ε
        decl     ::=  'var' ident ':' type ';'
        stmts    ::=  stmt stmts
                   |  ε
        stmt     ::=  ident '=' expr ';'
                   |  'read' ident ';'
                   |  'print' expr ';'
                   |  'while' expr 'do' stmts 'done'
        expr     ::=  term { '+' expr }
                   |  term { '-' expr }
                   |  term
        term     ::=  factor { '*' term }
                   |  factor { '-' term }
                   |  factor
        factor   ::=  '(' expr ')'
                   |  ident
                   |  int
                   |  float

    The AST nodes are represented with dicts as follows:
    - Declarations
        - var id: type         : { "nodetype": AST_DECL, "id": id, "type": type }

    - Statements
        - id = expr            : { "nodetype": AST_ASSIGN, "lhs": id, "rhs": expr }
        - print expr           : { "nodetype": AST_PRINT, "expr": expr }
        - read id              : { "nodetype": AST_READ, "id": id }
        - while e do stmts done: { "nodetype": AST_WHILE, "expr": e, "body": stmts }

    - Expressions
        - int                  : { "nodetype": AST_INT, "value": int }
        - float                : { "nodetype": AST_FLOAT, "value": float }
        - id                   : { "nodetype": AST_ID, "name": id }
        - e1 + e2              : { "nodetype": AST_BINOP, op: "+", "lhs": e1, "rhs": e2 }

    For example, here is a simple statement and its AST representation:

        x = 3 + y

        {
          "nodetype": AST_ASSIGN,
          "lhs": "x",
          "rhs": {
            "nodetype": AST_BINOP,
            "op": "+",
            "lhs": { "nodetype": AST_INT, "value": 3 },
            "rhs": { "nodetype": AST_ID, "name": "y" }
          }
        }
    """

    def consume(tok_type):
        if tok_type == toks[0]["toktype"]:
            t = toks.pop(0)
            return t
        else:
            error("expected %d, found %d" % (tok_type, toks[0]["toktype"]))

    def peek():
        if toks:
            return toks[0]["toktype"]
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
        decls = []
        while peek() == TOK_VAR:
            decls.append(decl())
        return decls

    def decl():
        if peek() == TOK_VAR:
            consume(TOK_VAR)
            id = consume(TOK_ID)
            consume(TOK_COLON)
            ty = consume(TOK_TYPE)
            consume(TOK_SEMI)
            return astnode(AST_DECL, id=id["value"], type=ty["value"])
        else:
            error("not a valid declaration")

    def stmts():
        stmts = []
        while peek() in (TOK_PRINT, TOK_READ, TOK_ID, TOK_WHILE):
            stmts.append(stmt())
        return stmts

    def stmt():
        next_tok = peek()
        if next_tok == TOK_ID:
            id = consume(TOK_ID)
            consume(TOK_EQ)
            e = expr()
            consume(TOK_SEMI)
            return astnode(AST_ASSIGN, lhs=id["value"], rhs=e)
        elif next_tok == TOK_PRINT:
            consume(TOK_PRINT)
            e = expr()
            consume(TOK_SEMI)
            return astnode(AST_PRINT, expr=e)
        elif next_tok == TOK_READ:
            consume(TOK_READ)
            id = consume(TOK_ID)
            consume(TOK_SEMI)
            return astnode(AST_READ, id=id)
        elif next_tok == TOK_WHILE:
            consume(TOK_WHILE)
            e = expr()
            consume(TOK_DO)
            body = stmts()
            consume(TOK_DONE)
            return astnode(AST_WHILE, expr=e, body=body)
        else:
            error("illegal statement")

    def expr():
        t = term()
        next_tok = peek()
        while next_tok in (TOK_PLUS, TOK_MINUS):
            if next_tok == TOK_PLUS:
                consume(TOK_PLUS)
                t2 = term()
                t = astnode(AST_BINOP, op="+", lhs=t, rhs=t2)
            elif next_tok == TOK_MINUS:
                consume(TOK_MINUS)
                t2 = term()
                t = astnode(AST_BINOP, op="-", lhs=t, rhs=t2)
            next_tok = peek()
        return t

    def term():
        f = factor()
        next_tok = peek()
        while next_tok in (TOK_STAR, TOK_SLASH):
            if next_tok == TOK_STAR:
                consume(TOK_STAR)
                f2 = factor()
                f = astnode(AST_BINOP, op="*", lhs=f, rhs=f2)
            elif next_tok == TOK_SLASH:
                consume(TOK_SLASH)
                f2 = factor()
                f = astnode(AST_BINOP, op="/", lhs=f, rhs=f2)
            next_tok = peek()
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
            return astnode(AST_INT, value=tok["value"])
        elif next_tok == TOK_FLOAT:
            tok = consume(TOK_FLOAT)
            return astnode(AST_FLOAT, value=tok["value"])
        elif next_tok == TOK_ID:
            tok = consume(TOK_ID)
            return astnode(AST_ID, name=tok["value"])
        else:
            error("illegal token %d" % next_tok)

    return program()


def build_symtab(ast):
    """
    Input : the AST of a mini program
    Output: a dictionary mapping variable names to types

    This procedure iterates over the declarations and adds them to a
    symbol table (here, a dictionary that maps variable names to their
    declared type).  If a variable is declared more than once, we
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
    information added inside expression nodes

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
        if stmt["nodetype"] == AST_PRINT:
            typed_expr = check_expr(stmt["expr"])
            return astnode(AST_PRINT, expr=typed_expr)
        elif stmt["nodetype"] == AST_READ:
            return astnode(AST_READ, id=stmt["id"])
        elif stmt["nodetype"] == AST_ASSIGN:
            typed_rhs = check_expr(stmt["rhs"])
            if typed_rhs["type"] == symtab[stmt["lhs"]]:
                return astnode(AST_ASSIGN, lhs=stmt["lhs"], rhs=typed_rhs)
            else:
                error("expected %s, got %s" % (symtab[stmt["lhs"]], typed_rhs["type"]))
        elif stmt["nodetype"] == AST_WHILE:
            typed_expr = check_expr(stmt["expr"])
            if typed_expr["type"] != "int":
                error("loop condition must be an int")
            typed_body = [check_stmt(body_stmt) for body_stmt in stmt["body"]]
            return astnode(AST_WHILE, expr=typed_expr, body=typed_body)

    def check_expr(expr):
        if expr["nodetype"] == AST_INT:
            return astnode(AST_INT, value=expr["value"], type="int")
        elif expr["nodetype"] == AST_FLOAT:
            return astnode(AST_FLOAT, value=expr["value"], type="float")
        elif expr["nodetype"] == AST_ID:
            if expr["name"] not in symtab:
                error("undeclared variable: %s" % expr["name"])
            return astnode(AST_ID, name=expr["name"], type=symtab[expr["name"]])
        elif expr["nodetype"] == AST_BINOP:
            typed_e1 = check_expr(expr["lhs"])
            typed_e2 = check_expr(expr["rhs"])
            if typed_e1["type"] == typed_e2["type"]:
                return astnode(AST_BINOP, op=expr["op"], lhs=typed_e1, rhs=typed_e2, type=typed_e1["type"])
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

    codegen(ast) will generate code for our Minilang program.  The code
    is clearly not optimal, nor even really human readable, however it
    is (a) correct, and (b) translated easily.

    For expressions, we pass the expression to translate, and
    gen_expr() prints the code for generating the expression
    and returns the name of the variable in which
    the result is stored.

    The new_temp() function creates a new temporary variable for every
    time it's called.

    A typical code generator would return a structure that could then
    be manipulated for analysis and optimization.
    """
    def new_temp():
        """Return a new, unique temporary variable name."""
        global curr_tmp
        curr_tmp += 1
        return "t_" + str(curr_tmp)

    def gen_decl(decl):
        print("%s %s;" % (decl["type"], decl["id"]))

    def gen_stmt(stmt):
        if stmt["nodetype"] == AST_ASSIGN:
            if stmt["lhs"] not in symtab:
                error("undeclared variable: %s" % stmt["lhs"])
            expr_loc = gen_expr(stmt["rhs"])
            print("%s = %s;" % (stmt["lhs"], expr_loc))
        elif stmt["nodetype"] == AST_PRINT:
            expr_loc = gen_expr(stmt["expr"])
            if stmt["expr"]["type"] == "int":
                flag = "d"
            else:
                flag = "f"
            print('printf("%%%s\\n", %s);' % (flag, expr_loc))
        elif stmt["nodetype"] == AST_READ:
            id = stmt["id"]["value"]
            if symtab[id] == "int":
                flag = "d"
            else:
                flag = "f"
            print('scanf("%%%s", &%s);' % (flag, id))
        elif stmt["nodetype"] == AST_WHILE:
            expr_loc = gen_expr(stmt["expr"])
            print("while (%s) { " % expr_loc)
            for body_stmt in stmt["body"]:
                gen_stmt(body_stmt)
            gen_expr(stmt["expr"], expr_loc)
            print("}")

    def gen_expr(expr, loc_name=None):
        if expr["nodetype"] in (AST_INT, AST_FLOAT):
            loc = loc_name or new_temp()
            print("%s %s = %s;" % (expr["type"], loc, expr["value"]))
            return loc
        elif expr["nodetype"] == AST_ID:
            return expr["name"]
        elif expr["nodetype"] == AST_BINOP:
            lhs_loc = gen_expr(expr["lhs"])
            rhs_loc = gen_expr(expr["rhs"])
            loc = new_temp()
            print("%s %s = %s %s %s;" % (expr["type"], loc, lhs_loc, expr["op"], rhs_loc))
            return loc

    # Add the usual C headers and main declaration.
    print("#include <stdio.h>")
    print("int main(void) {")

    # Add the variable declarations at the beginning of main.
    for decl in ast["decls"]:
        gen_decl(decl)

    # Add the C statements to the main function.
    for stmt in ast["stmts"]:
        gen_stmt(stmt)

    print("}")




def main():
    src = sys.stdin.read()
    toks = lex(src)                      # source -> tokens
    ast = parse(toks)                    # tokens -> AST
    symtab = build_symtab(ast)           # AST -> symbol table
    typed_ast = typecheck(ast, symtab)   # AST * symbol table -> Typed AST
    codegen(typed_ast, symtab)  # Typed AST * symbol table -> C code


if __name__ == "__main__":
    main()
