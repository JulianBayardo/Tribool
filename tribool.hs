{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module TriBool where

import Data.List (transpose, nub)
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
                expand k = foldr ((++) . replicate k) []

                repeatN :: Int -> [a] -> [a]
                repeatN 0 _ = []
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
