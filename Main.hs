module Main
where

import Mu2
import System

main = do
  args <- getArgs
  if length args < 1
     then putStrLn "Usage: mu2 filename"
     else do
       let filename = head args
       result <- runFile filename
       seq result (return ())
  return ()

