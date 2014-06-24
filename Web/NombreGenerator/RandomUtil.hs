module Web.NombreGenerator.RandomUtil (randTriples) where

import System.Random
import Control.Monad

pick :: [a] -> IO a
pick list = randomRIO (0, length list - 1) >>= return . (list !!)

takeRandom :: Int -> [a] -> IO [a]
takeRandom n list = replicateM n $ pick list

format :: [String] -> String
format [a,b,c] = a ++ ", " ++ b ++ " " ++ c

randTriples :: Int -> [String] -> IO [String]
randTriples n list = replicateM n (fmap format $ takeRandom 3 list)
