module Main where

import CLI

main :: IO ()
main = do
    path <- getArgs
    runFs path
