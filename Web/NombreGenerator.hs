module Main where

import Paths_hs_nombre_generator (version)

import Data.Version (showVersion)
import System.Environment
import System.Exit
import Web.NombreGenerator.Generator.Candidaturas
import Web.NombreGenerator.RandomUtil
import Web.NombreGenerator.Scrapper.BsAs

type Count = Int
type Sex = String

parseArgs :: [String] -> IO (Count, Sex)
parseArgs ["-h"] = printUsage >> exit
parseArgs ["-v"] = printVersion >> exit
parseArgs [n] = return $ evalArgs n "ambos"
parseArgs ["-m", n] = return $ evalArgs n "m"
parseArgs ["-f", n] = return $ evalArgs n "f"
parseArgs _ = printUsage >> die

evalArgs :: String-> String -> (Count, Sex)
evalArgs count sex = (read count :: Count) `seq` (read count, sex)

printUsage = putStrLn "Usage: hs-nombre-generator [-h] [-m/-f] [count ..]"
printVersion = putStrLn $ "NombreGenerator " ++ showVersion version
exit = exitSuccess
die = exitWith (ExitFailure 1)

main = getArgs >>=
       parseArgs >>=
       generate scrap >>=
       candidaturas [("INT", 1, False), ("CON", 3, True)] >>=
       mapM_ putStrLn
