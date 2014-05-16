import ThreeValued
import Parser
import System.Environment (getArgs)
import Text.Parsec (parse)

printTruthStatement :: Expr ThreeValued -> [VarMap] -> IO ()
printTruthStatement expression mapping = putStrLn $ x ++ show (exec mapping expression)
    where
        x = foldr (\(_, value) str -> str ++ show value ++ "\t") "" mapping

outputTruthTable :: String -> IO ()
outputTruthTable input =
    case parse threeValuedExpression "" input of
        Left err -> print err
        Right expression ->
            let
                variables = getVariables expression
                tTable = truthTable $ length variables
                varMap = fmap (zip variables) tTable
            in
            putStrLn (foldr (\name str -> str ++ name ++ "\t") "" variables) >>
            mapM_ (printTruthStatement expression) varMap


main :: IO ()
main = getArgs >>= mapM_ outputTruthTable
