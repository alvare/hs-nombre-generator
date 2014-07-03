module Main where

import Paths_hs_nombre_generator (version)

import Data.Version (showVersion)
import System.Environment
import System.Exit
import Web.NombreGenerator.RandomUtil
import Web.NombreGenerator.Scrapper.BsAs

parseArgs :: [String] -> IO (Int, String)
parseArgs ["-h"] = printUsage >> exit
parseArgs ["-v"] = printVersion >> exit
parseArgs [n] = return $ evalArgs n "ambos"
parseArgs ["-m", n] = return $ evalArgs n "m"
parseArgs ["-f", n] = return $ evalArgs n "f"
parseArgs _ = printUsage >> die

evalArgs :: String-> String -> (Int, String)
evalArgs count sex = (read count :: Int) `seq` (read count, sex)

printUsage = putStrLn "Usage: hs-nombre-generator [-h] [-m/-f] [count ..]"
printVersion = putStrLn $ "NombreGenerator " ++ showVersion version
exit = exitSuccess
die = exitWith (ExitFailure 1)

main = getArgs >>= parseArgs >>= generate scrap >>= mapM_ putStrLn
