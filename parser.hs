module Parser where

import Text.Parsec.Expr
import Text.Parsec
import ThreeValued

-- Maps operators to types.
table =
    [
    [Prefix $ (char '¬' >> return NOT) <|> (char '~' >> return NOT)],
    [Infix ((char '|' >> return OR) <|> (string "||" >> return OR)) AssocLeft],
    [Infix (char '+' >> return XOR) AssocLeft],
    [Infix ((char '^' >> return AND) <|> (char '&' >> return AND)) AssocLeft],
    [Infix ((string "<->" >> return IIF) <|> (string "<=>" >> return IIF)) AssocLeft],
    [Infix ((string "->" >> return THEN) <|> (string "=>" >> return THEN)) AssocLeft]
    ]

-- Thanks to https://stackoverflow.com/questions/7209260/checking-if-a-string-consists-of-balanced-parenthesis
braces x = choice [between (char o) (char c) x | (o,c) <- [('(',')'),('[',']'),('{','}')]]

{-| Defines what a formula is:
    * Must begin and end with (, {, [ and ], }, ) respectively.
    * May contain some sort of expression.
    * May have an alphanumeric value, in which case is supposed to be a variable name.
    * There can be any numer of spaces between any of the mentioned tokens.
-}
formula = braces (do { spaces; x <- threeValuedExpression; spaces; return x })
        <|> do {spaces; x <- many1 alphaNum; spaces; return (VARIABLE x) }
        <?> "atomic expression"

-- Builds a expression parser.
threeValuedExpression :: Parsec String () (Expr ThreeValued)
threeValuedExpression = buildExpressionParser table formula <?> "expression"
