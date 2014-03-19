Tribool
=======

Parses and executes triboolean expressions. Just compile the program using

    ghc -O3 -Wall -o tribool main.hs parser.hs tribool.hs

And run it with the expression(s) you want to evaluate as an argument, for example:

    ./tribool "a -> b"
    ./tribool "(a|b)&c"
    ./tribool "a|b&c"
    ./tribool "(a|b)&c" "a|b"
    ...

And it will generate truth tables for the expressions. The recognized operators
are: -> => (then), <-> <=> (iif), + (xor), | || (or), & ^ (and), ~ ¬ (not).
