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
evaluateStep s op = Err op : s

evaluateProgram :: [Expr] -> Stack
evaluateProgram = foldl evaluateStep []

someFunc :: IO ()
someFunc = print (evaluateProgram <$> parseProgram program1)

k :: IO ()
k = someFunc
