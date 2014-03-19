Tribool
=======

Parses and executes triboolean expressions. Just compile the program using ghc and run it with a format such as:

    ./tribool "a -> b"
    ./tribool "(a|b)&c"

and it will generate truth tables for the expressions. The recognized operators are: -> => (then), <-> <=> (iif), + (xor), | || (or), & ^ (and), ~ (not).
