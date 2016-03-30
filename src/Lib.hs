module Lib where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Functor.Identity
import           Text.Megaparsec
import           Text.Megaparsec.Lexer  hiding (space)

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

evaluateStep :: MonadIO m
             => Stack -> Expr -> m Stack
evaluateStep s (Value n) = pure (SValue n : s)
evaluateStep (SValue x:SValue y:s) (Symbol "+") = pure $  SValue (x + y) : s
evaluateStep (SValue x:SValue y:s) (Symbol "*") = pure $  SValue (x * y) : s
evaluateStep (SValue x:s) (Symbol "str") = pure $  SString (show x) : s
evaluateStep (SString x:s) (Symbol "str") = pure $  SString x : s
evaluateStep (SString x:s) (Symbol "reverse")= pure $  SString (reverse x) : s
evaluateStep s (Symbol "print") = liftIO (print s) >> pure s
evaluateStep s (Symbol "fib5") = pure $  (SValue <$> [1,2,3,5,8]) ++ s
evaluateStep s op = pure $  Err op : s

evaluateProgram :: [Expr] -> IO Stack
evaluateProgram = foldM evaluateStep []

runProgram :: String -> IO ()
runProgram text =
  do either print
            (void . evaluateProgram)
            (parseProgram text)
     print "----"

--         (intersperse (Symbol "print") <$> parseProgram text)
------------------------------------------------------------

programs :: [String]
programs =
  ["7 2 3 + * str reverse str print"
   ,"7 2 3 print + * str reverse str print"
   ,"fib5 print + + + + print"
    ,"1 + + print"
  ]

someFunc :: IO ()
someFunc = mapM_ runProgram programs

k :: IO ()
k = someFunc
