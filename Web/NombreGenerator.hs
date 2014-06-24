module Main where

import System.Environment
import System.Exit
import Web.NombreGenerator.Scrapper.BsAs

parseArgs :: [String] -> IO Int
parseArgs ["-h"] = usage >> exit
parseArgs ["-v"] = version >> exit
parseArgs [s] = return $! read s
parseArgs [] = usage >> die

usage = putStrLn "Usage: hs-nombre-generator [-h] [count ..]"
version = putStrLn "Haskell NombreGenerator 0.1"
exit = exitSuccess
die = exitWith (ExitFailure 1)

main = getArgs >>= parseArgs >>= takeNames >>= mapM_ putStrLn
