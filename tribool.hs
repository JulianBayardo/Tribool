{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
import Text.Parsec.Expr
import Text.Parsec
import System.Environment (getArgs)
import Data.List (nub, transpose)
import Data.Monoid

{- Defines TriBools (True, False, Bottom). -}
data TriBool = T | F | B deriving (Show, Read, Eq)

{-
    We are exploiting the fact that we can construct any boolean operator in
    terms of OR and NOT by defining less functions.
-}
not' :: TriBool -> TriBool
not' T = F
not' F = T
not' B = B

or' :: TriBool -> TriBool -> TriBool
or' T T = T
or' T F = T
or' F T = T
or' F F = F
or' T B = T
or' F B = B
or' B T = B
or' B F = B
or' B B = B

and' :: TriBool -> TriBool -> TriBool
and' p q = not' $ not' p `or'` not' q

then' :: TriBool -> TriBool -> TriBool
then' p q = not' p `or'` q

iif' :: TriBool -> TriBool -> TriBool
iif' p q = (p `then'` q) `and'` (q `then'` p)

xor' :: TriBool -> TriBool -> TriBool
xor' p q = (p `or'` q) `and'` not' (p `and'` q)

-- Generates a "matrix" of possible values for a number of variables. Each
-- row is a unique set of values for every variable.
truthTable :: Int -> [[TriBool]]
truthTable = transpose . gTruth
    where
        gTruth :: Int -> [[TriBool]]
        gTruth 1 = [[T, F, B]]
        gTruth n = one : two
            where
                prevTable = gTruth $ n-1
                one = expand 3 $ head prevTable
                two = fmap (repeatN 3) prevTable

                expand :: Int -> [a] -> [a]
                expand n = foldr ((++) . replicate n) []

                repeatN :: Int -> [a] -> [a]
                repeatN 0 list = []
                repeatN x list = list ++ repeatN (x-1) list

{-
    This data type is responsible for constructing valid language expressions.
    I know it's not necessary to use GADTs for this particular language, however,
    I'm using them just in case I want to add more features.
-}
data Expr a where
    VARIABLE :: String -> Expr TriBool
    NOT :: Expr TriBool -> Expr TriBool
    OR :: Expr TriBool -> Expr TriBool -> Expr TriBool
    XOR :: Expr TriBool -> Expr TriBool -> Expr TriBool
    AND :: Expr TriBool -> Expr TriBool -> Expr TriBool
    THEN :: Expr TriBool -> Expr TriBool -> Expr TriBool
    IIF :: Expr TriBool -> Expr TriBool -> Expr TriBool

instance Show (Expr TriBool) where
    show (VARIABLE x) = x
    show (NOT x) = '~' : show x
    show (OR x y) = mconcat ["(", show x, " | ", show y, ")"]
    show (AND x y) = mconcat ["(", show x, " && ", show y, ")"]
    show (THEN x y) = mconcat ["(", show x, " -> ", show y, ")"]
    show (IIF x y) = mconcat ["(", show x, " <-> ", show y, ")"]
    show (XOR x y) = mconcat ["(", show x, " + ", show y, ")"]

type VarMap = (String, TriBool)

-- Executes any expression which evaluates to a TriBool value.
exec :: [VarMap] -> Expr TriBool -> TriBool
exec [] (VARIABLE x) = error $ "Undefined variable " ++ x
exec (y:xs) (VARIABLE x)
    | fst y == x = snd y
    | otherwise = exec xs (VARIABLE x)
exec m (NOT x) = not' (exec m x)
exec m (OR x y) = or' (exec m x) (exec m y)
exec m (AND x y) = and' (exec m x) (exec m y)
exec m (THEN x y) = then' (exec m x) (exec m y)
exec m (IIF x y) = iif' (exec m x) (exec m y)
exec m (XOR x y) = xor' (exec m x) (exec m y)

-- Returns a list of variable names within an expression.
-- Each name appears only once.
getVariables :: Expr TriBool -> [String]
getVariables expr = reverse . nub $ getVariables' expr
    where
        getVariables' :: Expr TriBool -> [String]
        getVariables' (VARIABLE x) = [x]
        getVariables' (NOT x) = getVariables' x
        getVariables' (OR x y) = getVariables' x ++ getVariables' y
        getVariables' (AND x y) = getVariables' x ++ getVariables' y
        getVariables' (THEN x y) = getVariables' x ++ getVariables' y
        getVariables' (IIF x y) = getVariables' x ++ getVariables' y
        getVariables' (XOR x y) = getVariables' x ++ getVariables' y

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
formula = braces (do { spaces; x <- tribooleanExpression; spaces; return x })
        <|> do {spaces; x <- many1 alphaNum; spaces; return (VARIABLE x) }
        <?> "atomic expression"

-- Builds a expression parser.
tribooleanExpression :: Parsec String () (Expr TriBool)
tribooleanExpression = buildExpressionParser table formula <?> "expression"

printTruthStatement :: Expr TriBool -> [VarMap] -> IO ()
printTruthStatement expression mapping = do
    let x = foldr (\(_, value) str -> str ++ show value ++ "\t") "" mapping
    putStrLn $ x ++ show (exec mapping expression)
    where

outputTruthTable :: String -> IO ()
outputTruthTable input =
    case parse tribooleanExpression "" input of
        Left error -> print error
        Right expression ->
            let
                variables = getVariables expression
                table = truthTable $ length variables
                varMap = fmap (zip variables) table
            in
            putStrLn (foldr (\name str -> str ++ name ++ "\t") "" variables) >>
            mapM_ (printTruthStatement expression) varMap


main :: IO ()
main = getArgs >>= mapM_ outputTruthTable
