module Web.NombreGenerator.RandomUtil (generate) where

import Control.Monad
import Data.Char (toLower)
import System.Random

pick :: [a] -> IO a
pick list = randomRIO (0, length list - 1) >>= return . (list !!)

takeRandom :: Int -> [a] -> IO [a]
takeRandom n list = replicateM n $ pick list

format :: [(String, String)] -> (String, String)
format names_sex = (c ++ ", " ++ a ++ " " ++ b, sex)
    where [a, b, c] = map fst names_sex
          sex = snd . head $ names_sex

randTriples :: Int -> [(String, String)] -> IO [(String, String)]
randTriples n list = replicateM n (fmap format $ takeRandom 3 list)

generate :: IO [(String, String)] -> (Int, String) -> IO [(String, String)]
generate scrap (n, sex) = (fmap (filter isSex) . randTriples n) =<<  scrap
    where isSex (_, s) = sex == "ambos" || (lower s) == sex
          lower = map toLower
