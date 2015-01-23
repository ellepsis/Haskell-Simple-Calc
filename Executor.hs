module Executor where

import Parser
import Tokenizer
import qualified Data.Map as M

execute :: CalcTree -> Double

execute (SumNode op left right)  = 
    let leftRes = execute left 
        rigthRes = execute right 
    in
        case op of
          Plus  -> leftRes + rigthRes
          Minus -> leftRes - rigthRes

execute (ProductNode op left right)  = 
    let leftRes = execute left 
        rigthRes = execute right 
    in
        case op of
          Mul -> leftRes * rigthRes
          Div -> leftRes / rigthRes
          Pow -> leftRes ** rigthRes

execute (UnaryNode op tree) =
    let x  = execute tree
    in case op of
         Plus  -> x
         Minus -> -x

execute (NumNode x) = x
