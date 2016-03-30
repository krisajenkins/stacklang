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
  | Plus
  | Multiply
  | Str
  | Reverse
    deriving (Show,Eq)

------------------------------------------------------------

program1 :: String
program1 = "7 2 3 + * str reverse str"

int :: ParsecT String Identity Expr
int = Value <$> integer

plus :: ParsecT String Identity Expr
plus = string "+" *> pure Plus

multiply :: ParsecT String Identity Expr
multiply = string "*" *> pure Multiply

str :: ParsecT String Identity Expr
str = string "str" *> pure Str

reverseP :: ParsecT String Identity Expr
reverseP = string "reverse" *> pure Reverse

whitespace :: ParsecT String Identity ()
whitespace = skipMany spaceChar

expr :: ParsecT String Identity Expr
expr = choice [int,plus,multiply,str,reverseP] <* whitespace

programParser :: ParsecT String Identity [Expr]
programParser = some expr <* eof

parseProgram :: String -> Either ParseError [Expr]
parseProgram = runParser programParser "<code>"

------------------------------------------------------------

evaluateStep :: Stack -> Expr -> Stack
evaluateStep s (Value n) = SValue n : s
evaluateStep (SValue x:SValue y:s) Plus = SValue (x + y) : s
evaluateStep (SValue x:SValue y:s) Multiply = SValue (x * y) : s
evaluateStep (SValue x:s) Str = SString (show x) : s
evaluateStep (SString x:s) Str = SString x : s
evaluateStep (SString x:s) Reverse = SString (reverse x) : s
evaluateStep s op = Err op : s

evaluateProgram :: [Expr] -> Stack
evaluateProgram = foldl evaluateStep []

someFunc :: IO ()
someFunc = print (evaluateProgram <$> parseProgram program1)

k :: IO ()
k = someFunc
