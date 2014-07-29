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

parseArgs :: [String] -> IO ([Cargo], Sex)
parseArgs ["-h"] = printUsage >> exit
parseArgs ["-v"] = printVersion >> exit
parseArgs ["-f", cargs] = return $ eval (cargs, "f")
parseArgs ["-m", cargs] = return $ eval (cargs, "m")
parseArgs [cargs] = return $ eval (cargs, "a")
parseArgs _ = printUsage >> die

eval (c, s) = (read c :: [Cargo]) `seq` (read c :: [Cargo], s)

printUsage = putStrLn "Usage: hs-nombre-generator [-h] [-m/-f] '[('CARGO', Int, False|True), etc]'"
printVersion = putStrLn $ "NombreGenerator " ++ showVersion version
exit = exitSuccess
die = exitWith (ExitFailure 1)

format :: [(String, String)] -> (String, String)
format names_sex = (c ++ ", " ++ a ++ " " ++ b, custom_sex)
    where [a, b, c] = map fst names_sex
          sex = snd . head $ names_sex
          custom_sex = if sex == "A" then rand_sex else sex
          rand_sex = if (head a > 'K') then "M" else "F"

randTriples :: Int -> [(String, String)] -> IO [(String, String)]
randTriples n list = replicateM n (fmap format $ takeRandom 3 list)

generate :: IO [Name] -> Sex -> [Cargo] -> IO [FullName]
generate scrap sex cargos = (fmap (filter isSex) . randTriples count) =<<  scrap
    where isSex (_, s) = sex == "a" || (lower s) == sex
          lower = map toLower
          count = sum . map (\(_, n, supl) -> if supl then n * 2 else n) $ cargos

main = getArgs >>=
       parseArgs >>= \(cargos, sex) ->
           generate scrap sex cargos >>=
           candidaturas cargos >>=
           mapM_ putStrLn
