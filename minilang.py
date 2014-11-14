import sys

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


def error(msg):
    print("Error: " + msg)
    sys.exit(1)


def lex(s):
    """
    Input : a string representing a mini program
    Output: a list of tokens

    lex(s) will produce a sequence of tokens, which are tuples of two
    elements: a type of token (defined above) and a semantic value.
    The semantic value is a piece of information associated with the
    token, such as the name of an identifier or the value of an
    integer literal.  Some tokens, such as the plus symbol, do not
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
            tokens.append((TOK_EQ, None))
        elif c == "+":
            tokens.append((TOK_PLUS, None))
        elif c == "*":
            tokens.append((TOK_STAR, None))
        elif c == "(":
            tokens.append((TOK_LPAREN, None))
        elif c == ")":
            tokens.append((TOK_RPAREN, None))
        elif c == ":":
            tokens.append((TOK_COLON, None))

        # Integer literals
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
                tokens.append((TOK_FLOAT, float(num)))
            else:
                i -= 1
                tokens.append((TOK_INT, int(num)))

        # Identifiers and keywords
        elif c.isalpha() or c == "_":
            ident = ""
            while s[i].isalnum() or s[i] == "_":
                ident += s[i]
                i += 1
            i -= 1 # Read one char too many
            if ident == "print":
                tokens.append((TOK_PRINT, None))
            elif ident == "var":
                tokens.append((TOK_VAR, None))
            elif ident in ("int", "float"):
                tokens.append((TOK_TYPE, ident))
            else:
                tokens.append((TOK_ID, ident))
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

        program ::= { decl } { stmt }
        decl ::= ident ':' type
        stmt ::= ident '=' expr
               | 'print' expr
        expr ::= term '+' expr
               | term
        term ::= factor '*' term
               | factor
        factor ::= '(' expr ')'
                 | ident
                 | int

    The AST nodes are represented with tuples as follows:
    - Declarations
        - var x: int     : (TOK_VAR, x, int)
        - var y: float   : (TOK_VAR, y, float)

    - Statements
        - id = expr      : (TOK_EQ, id, expr)
        - print expr     : (TOK_PRINT, expr)

    - Expressions
        - int            : (TOK_INT, int)    # the actual token
        - id             : (TOK_ID, id)      # the actual token
        - e1 + e2        : (TOK_PLUS, e1, e2)
        - e1 * e2        : (TOK_STAR, e1, e2)

    For example, here is a simple statement and its AST representation:

        x = 3 + y
        (TOK_EQ, "x", (TOK_PLUS, (TOK_INT, 3), (TOK_ID, 4)))
    """

    def consume(tok_type):
        if tok_type == toks[0][0]:
            t = toks.pop(0)
            return t
        else:
            error("expected %d, found %d" % (tok_type, toks[0][0]))

    def peek():
        if toks:
            return toks[0][0]
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
            _, id = consume(TOK_ID)
            consume(TOK_COLON)
            _, ty = consume(TOK_TYPE)
            return (TOK_VAR, id, ty)
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
            _, id = consume(TOK_ID)
            consume(TOK_EQ)
            e = expr()
            return (TOK_EQ, id, e)
        elif next_tok == TOK_PRINT:
            consume(TOK_PRINT)
            e = expr()
            return (TOK_PRINT, e)
        else:
            error("illegal statement")

    def expr():
        t = term()
        next_tok = peek()
        if next_tok == TOK_PLUS:
            consume(TOK_PLUS)
            e = expr()
            return (TOK_PLUS, t, e)
        return t

    def term():
        f = factor()
        next_tok = peek()
        if next_tok == TOK_STAR:
            consume(TOK_STAR)
            t = term()
            return (TOK_STAR, f, t)
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
            return tok
        elif next_tok == TOK_FLOAT:
            tok = consume(TOK_FLOAT)
            return tok
        elif next_tok == TOK_ID:
            tok = consume(TOK_ID)
            return tok
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
    for _, id, ty in ast["decls"]:
        if id in symtab:
            error("%s is already declared" % id)
        else:
            symtab[id] = ty
    return symtab


def typecheck(ast, symtab):
    def check_stmt(stmt):
        if stmt[0] == TOK_PRINT:
            typed_expr = check_expr(stmt[1])
            return (TOK_PRINT, typed_expr)
        elif stmt[0] == TOK_EQ:
            typed_rhs = check_expr(stmt[2])
            if typed_rhs[-1] == symtab[stmt[1]]:
                return (TOK_EQ, stmt[1], typed_rhs)
            else:
                error("expected %s, got %s" % (symtab[stmt[1]], typed_rhs[-1]))

    def check_expr(expr):
        if expr[0] == TOK_INT:
            return (TOK_INT, expr[1], "int")
        elif expr[0] == TOK_FLOAT:
            return (TOK_FLOAT, expr[1], "float")
        elif expr[0] == TOK_ID:
            return (TOK_ID, expr[1], symtab[expr[1]])
        elif expr[0] in (TOK_PLUS, TOK_STAR):
            typed_e1 = check_expr(expr[1])
            typed_e2 = check_expr(expr[2])
            if typed_e1[-1] == typed_e2[-1]:
                return (expr[0], typed_e1, typed_e2, typed_e1[-1])
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

    codegen(ast) will generate code in three-address code.
    """
    def new_temp():
        global curr_tmp
        curr_tmp += 1
        return "tmp_" + str(curr_tmp)

    def gen_stmt(stmt):
        if stmt[0] == TOK_EQ:
            if stmt[1] not in symtab:
                error("undeclared variable: %s" % stmt[1])
            tmp = new_temp()
            expr_out = gen_expr(stmt[2], tmp)
            return expr_out + ["%s = %s;" % (stmt[1], tmp)]
        elif stmt[0] == TOK_PRINT:
            tmp = new_temp()
            expr_out = gen_expr(stmt[1], tmp)
            if stmt[1][-1] == "int":
                flag = "d"
            else:
                flag = "f"
            return expr_out + ['printf("%%%s\\n", %s);' % (flag, tmp)]

    def gen_expr(expr, result_register):
        if expr[0] in (TOK_INT, TOK_FLOAT):
            return ["%s %s = %s;" % (expr[2], result_register, expr[1])]
        elif expr[0] == TOK_ID:
            if expr[1] not in symtab:
                error("undeclared variable: %s" % expr[1])
            return ["%s %s = %s;" % (expr[2], result_register, expr[1])]
        elif expr[0] in (TOK_PLUS, TOK_STAR):
            t1 = new_temp()
            e1 = gen_expr(expr[1], t1)
            t2 = new_temp()
            e2 = gen_expr(expr[2], t2)
            if expr[0] == TOK_PLUS:
                op = "+"
            else:
                op = "*"
            return e1 + e2 + ["%s %s = %s %s %s;" % (expr[3], result_register, t1, op, t2)]

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
    toks = lex(src)                # source -> tokens
    ast = parse(toks)              # tokens -> AST
    symtab = build_symtab(ast)     # AST -> symbol table
    typed_ast = typecheck(ast, symtab)
    for s in codegen(typed_ast, symtab): # (AST * symbol table) -> C code
        print(s)

if __name__ == "__main__":
    main()
