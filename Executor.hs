module Executor where

import Parser
import Tokenizer

execute :: CalcTree -> Double

execute (BinNode op left right)  = 
    let leftRes = execute left 
        rigthRes = execute right 
    in
        case op of
          Plus  -> leftRes + rigthRes
          Minus -> leftRes - rigthRes
          Mul -> leftRes * rigthRes
          Div -> leftRes / rigthRes
          Pow -> leftRes ** rigthRes
          
execute (UnaryNode op tree) =
    let x  = execute tree
    in case op of
         Plus  -> x
         Minus -> -x
         Log -> log x

execute (NumNode x) = x
