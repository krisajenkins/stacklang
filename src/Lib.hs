module Lib where

import           Data.Functor.Identity

import           Text.Megaparsec
import           Text.Megaparsec.Lexer hiding (space)

data StackElement
  = SValue Integer
  | SString String
  | Err Expr
  deriving (Show,Eq)

type Stack = [StackElement]

data Expr
  = Value Integer
  | Symbol String
    deriving (Show,Eq)

------------------------------------------------------------

program1 :: String
program1 = "7 2 3 + * str reverse str"

program2 :: String
program2 = "fib5 + + + +"

int :: ParsecT String Identity Expr
int = Value <$> integer

symbolP :: ParsecT String Identity Expr
symbolP = Symbol <$> choice [some alphaNumChar,string "+",string "*"]

whitespace :: ParsecT String Identity ()
whitespace = skipMany spaceChar

expr :: ParsecT String Identity Expr
expr = choice [int,symbolP] <* whitespace

programParser :: ParsecT String Identity [Expr]
programParser = some expr <* eof

parseProgram :: String -> Either ParseError [Expr]
parseProgram = runParser programParser "<code>"

------------------------------------------------------------

evaluateStep :: Stack -> Expr -> Stack
evaluateStep s (Value n) = SValue n : s
evaluateStep (SValue x:SValue y:s) (Symbol "+") = SValue (x + y) : s
evaluateStep (SValue x:SValue y:s) (Symbol "*") = SValue (x * y) : s
evaluateStep (SValue x:s) (Symbol "str") = SString (show x) : s
evaluateStep (SString x:s) (Symbol "str") = SString x : s
evaluateStep (SString x:s) (Symbol "reverse")= SString (reverse x) : s
evaluateStep s (Symbol "fib5") = (SValue <$> [1,2,3,5,8]) ++ s
evaluateStep s op = Err op : s

evaluateProgram :: [Expr] -> [Stack]
evaluateProgram = scanl evaluateStep []

runProgram :: String -> Either ParseError [Stack]
runProgram text = evaluateProgram <$> parseProgram text

------------------------------------------------------------

someFunc :: IO ()
someFunc =
  do either print
            (mapM_ print)
            (runProgram program1)
     print "----"
     either print
            (mapM_ print)
            (runProgram program2)

k :: IO ()
k = someFunc
