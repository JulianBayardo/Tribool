Three-Valued
============

Parses and executes ternary logic expressions. Just compile the program using

    ghc -O3 -Wall -o threevalued main.hs parser.hs defs.hs

And run it with the expression(s) you want to evaluate as an argument, for example:

    ./threevalued "a -> b"
    ./threevalued "(a|b)&c"
    ./threevalued "a|b&c"
    ./threevalued "(a|b)&c" "a|b"
    ...

It will then generate truth tables for the expressions. The recognized operators
are: -> => (then), <-> <=> (iif), + (xor), | || (or), & ^ (and), ~ ¬ (not).

Please notice that spaces between expressions are currently not parsed properly, they will be fixed soon.
