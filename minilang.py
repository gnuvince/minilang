import sys

TOK_SET    = 0
TOK_PRINT  = 1
TOK_ID     = 2
TOK_INT    = 3
TOK_EQ     = 4
TOK_PLUS   = 5
TOK_STAR   = 6
TOK_LPAREN = 7
TOK_RPAREN = 8

def error(msg):
    print(msg)
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

        # Integer literals
        elif c.isdigit():
            num = ""
            while s[i].isdigit():
                num += s[i]
                i += 1
            i -= 1 # Read one char too many
            tokens.append((TOK_INT, int(num)))

        # Identifiers and keywords
        elif c.isalpha() or c == "_":
            ident = ""
            while s[i].isalnum() or s[i] == "_":
                ident += s[i]
                i += 1
            i -= 1 # Read one char too many
            if ident == "set":
                tokens.append((TOK_SET, None))
            elif ident == "print":
                tokens.append((TOK_PRINT, None))
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

        program ::= { stmt }
        stmt ::= 'set' ident '=' expr
               | 'print' expr
        expr ::= term '+' expr
               | term
        term ::= factor '*' term
               | factor
        factor ::= '(' expr ')'
                 | ident
                 | int

    The AST nodes are represented with tuples as follows:
    - Statements
        - set id = expr  : (TOK_SET, id, expr)
        - print expr     : (TOK_PRINT, expr)

    - Expressions
        - int            : (TOK_INT, int)    # the actual token
        - id             : (TOK_ID, id)      # the actual token
        - e1 + e2        : (TOK_PLUS, e1, e2)
        - e1 * e2        : (TOK_STAR, e1, e2)

    For example, here is a simple statement and its AST representation:

        set x = 3 + y
        (TOK_SET, "x", (TOK_PLUS, (TOK_INT, 3), (TOK_ID, 4)))
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
        ast = []
        while toks:
            ast.append(stmt())
        return ast

    def stmt():
        next_tok = peek()
        if next_tok == TOK_SET:
            consume(TOK_SET)
            _, id = consume(TOK_ID)
            consume(TOK_EQ)
            e = expr()
            return (TOK_SET, id, e)
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
        elif next_tok == TOK_ID:
            tok = consume(TOK_ID)
            return tok
        else:
            error("illegal token %d" % next_tok)

    return program()


curr_tmp = 0
def codegen(ast):
    """
    Input : the AST of a mini program
    Output: an equivalent C program

    codegen(ast) will generate code in three-address code.
    """
    def new_temp():
        global curr_tmp
        curr_tmp += 1
        return "tmp_" + str(curr_tmp)

    def gen_stmt(stmt):
        if stmt[0] == TOK_SET:
            tmp = new_temp()
            expr_out = gen_expr(stmt[2], tmp)
            return expr_out + ["%s = %s;" % (stmt[1], tmp)]
        elif stmt[0] == TOK_PRINT:
            tmp = new_temp()
            expr_out = gen_expr(stmt[1], tmp)
            return expr_out + ['printf("%%d\\n", %s);' % tmp]

    def gen_expr(expr, result_register):
        if expr[0] == TOK_INT:
            return ["int %s = %s;" % (result_register, expr[1])]
        elif expr[0] == TOK_ID:
            return ["int %s = %s;" % (result_register, expr[1])]
        elif expr[0] == TOK_PLUS:
            t1 = new_temp()
            e1 = gen_expr(expr[1], t1)
            t2 = new_temp()
            e2 = gen_expr(expr[2], t2)
            return e1 + e2 + ["int %s = %s + %s;" % (result_register, t1, t2)]
        elif expr[0] == TOK_STAR:
            t1 = new_temp()
            e1 = gen_expr(expr[1], t1)
            t2 = new_temp()
            e2 = gen_expr(expr[2], t2)
            return e1 + e2 + ["int %s = %s * %s;" % (result_register, t1, t2)]

    output = []

    # Add the usual C headers and main declaration.
    output += ["#include <stdio.h>"]
    output += ["int main(void) {"]

    # Find the set statements, and add declarations for their ids at
    # the top of the main function.
    decls = set([])
    for stmt in ast:
        if stmt[0] == TOK_SET:
            decls.add(stmt[1])
    for decl in decls:
        output += ["int %s;" % decl]

    # Add the C statements to the main function.
    for stmt in ast:
        output += gen_stmt(stmt)

    output += ["}"]
    return output




def main():
    src = sys.stdin.read()      # source -> tokens
    toks = lex(src)             # tokens -> AST
    ast = parse(toks)           # AST -> C code
    for s in codegen(ast):
        print(s)

if __name__ == "__main__":
    main()
