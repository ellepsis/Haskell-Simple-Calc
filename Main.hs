module Main where

import  Executor
import  Parser
import  Tokenizer
import  Control.Exception
import  Control.Applicative
import  Control.Monad
import  Data.Either

main = do
	input <- getLine 
	when (input == "exit") (return ())
	res <- try ((execute . parse . tokenize ) <$> (return input)) :: IO (Either SomeException Double)
	case res of 
		Left exc -> do
		 	putStrLn $ "Caught exception: " ++ show exc
		 	main 
		Right val -> do 
			putStrLn $ "The answer is: " ++ show val
			main