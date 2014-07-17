module Web.NombreGenerator.RandomUtil (takeRandom) where

import Control.Monad
import System.Random

pick :: [a] -> IO a
pick list = randomRIO (0, length list - 1) >>= return . (list !!)

takeRandom :: Int -> [a] -> IO [a]
takeRandom n list = replicateM n $ pick list
