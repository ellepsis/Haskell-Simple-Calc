module Tokenizer where

import Data.Char

data Operator = Plus | Minus | Mul | Div | Pow | Log
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokIdent String
           | TokNum Double
           | TokLParen
           | TokRParen
           | TokEnd
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mul
           | c == '/' = Div
           | c == '^' = Pow

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs) 
    | elem c "+-*/^" = TokOp (operator c) : tokenize cs
    | isDigit c = number c cs
    | isAlpha c = operation c cs
    | isSpace c = tokenize cs
    | c == '('  = TokLParen : tokenize cs
    | c == ')'  = TokRParen : tokenize cs
    | otherwise = error $ "Cannot tokenize: " ++ [c]

operation c cs = let (str, cs') = span isAlphaNum cs in
  if ((c:str) == "log") 
    then (TokOp Log) : tokenize cs'
    else error $ "Cannot tokenize: " ++ (c:str)

number c cs = 
   let (digs, cs') = span isDigit cs in
   TokNum (read (c : digs)) : tokenize cs'
