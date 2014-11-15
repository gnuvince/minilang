# Minilang

This is a one-file compiler for a not-even-Turing-complete language.
It is meant to show to students that implementing a compiler need not
be a scary affair: it can be done in a couple hundred lines of code in
a friendly language like Python.

minilang.py reads a mini program from stdin and outputs an equivalent
C program on stdout, which can then be fed into a C compiler such as
gcc:

    $ python minilang.py < demos/demo1.mini | gcc -x c -o /tmp/demo1 -
    $ /tmp/demo1

minilang.py should work with either Python 2 or Python 3.
