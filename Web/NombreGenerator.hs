module Main where

import Paths_hs_nombre_generator (version)

import Control.Monad (replicateM)
import Data.Char (toLower)
import Data.Version (showVersion)
import System.Environment
import System.Exit
import Web.NombreGenerator.Generator.Candidaturas
import Web.NombreGenerator.RandomUtil
import Web.NombreGenerator.Scrapper.BsAs

type Sex = String

parseArgs :: [String] -> IO Sex
parseArgs ["-h"] = printUsage >> exit
parseArgs ["-v"] = printVersion >> exit
parseArgs ["-f"] = return $ "f"
parseArgs ["-m"] = return $ "m"
parseArgs [] = return $ "a"
parseArgs _ = printUsage >> die

printUsage = putStrLn "Usage: hs-nombre-generator [-h] [-m/-f] [todo: candidaturas]"
printVersion = putStrLn $ "NombreGenerator " ++ showVersion version
exit = exitSuccess
die = exitWith (ExitFailure 1)


format :: [(String, String)] -> (String, String)
format names_sex = (c ++ ", " ++ a ++ " " ++ b, sex)
    where [a, b, c] = map fst names_sex
          sex = snd . head $ names_sex

randTriples :: Int -> [(String, String)] -> IO [(String, String)]
randTriples n list = replicateM n (fmap format $ takeRandom 3 list)

--TODO FUCK IO
generate :: IO [Name] -> Sex -> IO [FullName]
generate scrap sex = (fmap (filter isSex) . randTriples 100) =<<  scrap
    where isSex (_, s) = sex == "a" || (lower s) == sex
          lower = map toLower

main = getArgs >>=
       parseArgs >>=
       generate scrap >>=
       candidaturas [("INT", 1, False), ("CON", 3, True)] >>=
       mapM_ putStrLn
