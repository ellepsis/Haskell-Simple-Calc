module Parser where

import Tokenizer

-- Priority 
-- 1. Unary: Minus (-), Plus (+), Log (log)
-- 2. Power (^)
-- 3. Multiply (*), Divide (/)
-- 4. Minus (-)
-- 5. Plus (+)

data CalcTree = BinNode Operator CalcTree CalcTree
          | UnaryNode Operator CalcTree
          | NumNode Double
  deriving Show

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (c:cs) = c

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (t:ts) = ts

parse :: [Token] -> CalcTree
parse tokens = let (tree, tokens') = expression tokens
             in
               if null tokens' 
               then tree
               else error $ "Leftover tokens: " ++ show tokens'

expression :: [Token] -> (CalcTree, [Token])
expression tokens = 
   let (termTree, tokens') = expressionMinus tokens
   in
      case lookAhead tokens' of
        -- Term [+]
        (TokOp op) | op == Plus -> 
            let (exTree, tokens'') = expression (accept tokens') 
            in (BinNode op termTree exTree, tokens'')
        _ -> (termTree, tokens')

expressionMinus :: [Token] -> (CalcTree, [Token])
expressionMinus tokens = 
    let (termTree, tokens') = term tokens
    in 
      case lookAhead tokens' of
        -- Term [-]
        (TokOp op) | op == Minus -> 
            let (exTree, tokens'') = expressionMinus (accept tokens') 
            in (BinNode op termTree exTree, tokens'')
        _ -> (termTree, tokens')

term :: [Token] -> (CalcTree, [Token])
term tokens = 
   let (facTree, tokens') = termPow tokens
   in
      case lookAhead tokens' of
        -- Term [*/]
        (TokOp op) | elem op [Mul, Div] ->
            let (termTree, tokens'') = term (accept tokens') 
            in (BinNode op facTree termTree, tokens'')
        _ -> (facTree, tokens')

termPow :: [Token] -> (CalcTree, [Token])
termPow tokens = 
   let (facTree, tokens') = factor tokens
   in
      case lookAhead tokens' of
        -- Term [^]
        (TokOp op) | op == Pow ->
            let (termTree, tokens'') = termPow (accept tokens') 
            in (BinNode op facTree termTree, tokens'')
        _ -> (facTree, tokens')

factor :: [Token] -> (CalcTree, [Token])
factor tokens = 
   case lookAhead tokens of
      (TokNum x) -> (NumNode x, accept tokens)
      (TokOp op) | elem op [Plus, Minus, Log] -> 
            let (facTree, tokens') = factor (accept tokens) 
            in (UnaryNode op facTree, tokens')
      TokLParen -> 
         let (expTree, tokens') = expression (accept tokens)
         in
            if lookAhead tokens' /= TokRParen 
            then error "Missing right parenthesis"
            else (expTree, accept tokens')
      _ -> error $ "Parse error on token: " ++ show tokens

