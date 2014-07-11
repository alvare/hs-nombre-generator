module Web.NombreGenerator.Generator.Candidaturas (candidaturas) where

import Text.Printf
import Web.NombreGenerator.Scrapper.BsAs

type Categoria = (String, Int, Bool)

data Candidato = Candidato { partido :: Int
                           , lista :: Int
                           , uid :: Int
                           , nombre :: String
                           , orden :: Int
                           , titular :: Bool
                           , sexo :: String
                           } deriving (Show)

candidato_format = "('%03d.%03d.%03d', 'Candidato', '%s', '%s', 'NO', 'SI', '{\"norden\": %d, \"titular\": \"%s\", \"sexo\": \"%s\"}'),"

candidaturas :: [Categoria] -> [Name] -> IO [String]
candidaturas cats names = return (map printCand (loop1 [] 1 cats names))

printCand :: Candidato -> String
printCand (Candidato p l i n o t' s) = printf candidato_format p l i n n o t s
    where t = if t' then "SI" else "NO"

loop1 :: [Candidato] -> Int -> [Categoria] -> [Name] -> [Candidato]
loop1 acc idx (cat:xs) names = loop1 (catCands ++ acc) newIdx xs names
    where (catCands, newIdx) = loop2 [] cat 0 idx names
loop1 acc _ [] _ = acc

loop2 :: [Candidato] -> Categoria -> Int -> Int -> [Name] -> ([Candidato], Int)
loop2 acc cat orden idx names
    | supl && orden == maxOrd = loop2 (newCand : acc) (candName, maxOrd, False) 0 (idx + 1) names
    | orden == maxOrd = (acc, idx)
    | otherwise = loop2 (newCand : acc) cat (orden + 1) (idx + 1) names
    where newCand = Candidato {partido=1, lista=1, uid=idx, nombre=name, orden=(orden + 1), titular=supl, sexo=sex}
          (name, sex) = names !! idx
          (candName, maxOrd, supl) = cat
