module Web.NombreGenerator.Generator.Candidaturas (Cargo, FullName, candidaturas, candidaturas2) where

import Text.Printf
import Control.Monad
import Control.Monad.Trans.State.Lazy

type FullName = (String, String)
type Cargo = (String, Int, Bool)

data Candidato = Candidato { uid :: Int
                           , lista :: Int
                           , cargo :: String
                           , nombre :: String
                           , sexo :: String
                           , titular :: Bool
                           , orden :: Int
                           } deriving (Show)

candidato_format_sql = "('001.%03d.%03d', 'Candidato', '%s', '%s', 'NO', 'SI', '{\"norden\": %d, \"titular\": \"%s\", \"sexo\": \"%s\"}'),"
candidato_format_csv = "%03d,%03d,%s,,,EJ,\"%s\",%s,%s,,NO,SI,%d"

fromatCand :: Candidato -> String
fromatCand (Candidato i l n c s t' o) = printf candidato_format_csv i l n c s t o
    where t = if t' then "SI" else "NO"

candidaturas :: [Cargo] -> Int -> [FullName] -> IO [String]
candidaturas cats listas names = return $ reverse $ map fromatCand $ loop1 [] 1 cats names

loop1 :: [Candidato] -> Int -> [Cargo] -> [FullName] -> [Candidato]
loop1 acc _ [] _ = acc
loop1 acc idx (cat:xs) names = loop1 (catCands ++ acc) newIdx xs names
    where (catCands, newIdx) = loop2 [] 1 idx names 1 cat

loop2 :: [Candidato] -> Int -> Int -> [FullName] -> Int -> Cargo -> ([Candidato], Int)
loop2 acc orden idx names ln cat
    | orden > max_ord && is_suplent = (acc, idx)
    | orden > max_ord && has_suplent = loop2 acc 1 idx names ln (carg_name, max_ord, False)
    | otherwise = loop2 (newCand : acc) (orden + 1) (idx + 1) names ln cat
    where (carg_name, max_ord, has_suplent) = cat
          is_suplent = not has_suplent
          (name, sex) = names !! (idx - 1)
          newCand = Candidato {cargo=carg_name,
                               lista=ln,
                               uid=idx,
                               nombre=name,
                               orden=orden,
                               titular=is_suplent,
                               sexo=sex}

candidaturas2 :: [Cargo] -> Int -> [FullName] -> [String]
candidaturas2 cats listas names = reverse . map fromatCand . concat $ evalState lol 1
    where lol :: State Int [[Candidato]]
          lol = forM [1..listas] (\ln -> do
                                            idx <- get
                                            let candxs = map (loop2 [] 1 idx names ln) cats
                                            put . snd . last $ candxs
                                            return . concat . map fst $ candxs)
