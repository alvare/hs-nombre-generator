module Web.NombreGenerator.RandomUtil (randTriples) where

import Data.Random (runRVar)
import Data.Random.Extras
import Data.Random.Source.Std
import Control.Monad

takeRandom :: Int -> [a] -> IO [a]
takeRandom n list = runRVar (sample n list) StdRandom

format :: [String] -> String
format [a,b,c] = a ++ ", " ++ b ++ " " ++ c

randTriples :: Int -> [String] -> IO [String]
randTriples n list = replicateM n (fmap format $ takeRandom 3 list)
