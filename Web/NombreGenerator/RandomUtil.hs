{-# LANGUAGE BangPatterns #-}
module Web.NombreGenerator.RandomUtil (takeRandom) where

import Data.Random (runRVar)
import Data.Random.Extras
import Data.Random.Source.Std

takeRandom :: Int -> [a] -> IO [a]
takeRandom n list = runRVar (sample n list) StdRandom
