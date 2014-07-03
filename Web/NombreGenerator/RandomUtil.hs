module Web.NombreGenerator.RandomUtil (generate) where

import Control.Monad
import Data.Char (toLower)
import System.Random

pick :: [a] -> IO a
pick list = randomRIO (0, length list - 1) >>= return . (list !!)

takeRandom :: Int -> [a] -> IO [a]
takeRandom n list = replicateM n $ pick list

format :: [String] -> String
format [a,b,c] = a ++ ", " ++ b ++ " " ++ c

randTriples :: Int -> [String] -> IO [String]
randTriples n list = replicateM n (fmap format $ takeRandom 3 list)

generate scrap (n, sex) = (randTriples n . firsts . filter isSex) =<<  scrap
    where isSex (_, s) = sex == "ambos" || (lower s) == sex
          lower = map toLower
          firsts = map fst
