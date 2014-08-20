module Web.NombreGenerator.Generator.Candidaturas (Cargo, FullName, candidaturas) where

import Text.Printf

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

candidato_format_sql = "('%03d.%03d.%03d', 'Candidato', '%s', '%s', 'NO', 'SI', '{\"norden\": %d, \"titular\": \"%s\", \"sexo\": \"%s\"}'),"
candidato_format_csv = "%03d,%03d,%s,,,EJ,\"%s\",%s,%s,,NO,SI,%d"

candidaturas :: [Cargo] -> [FullName] -> IO [String]
candidaturas cats names = return $ reverse $ map fromatCand $ loop1 [] 1 cats names

fromatCand :: Candidato -> String
fromatCand (Candidato i l n c s t' o) = printf candidato_format_csv i l n c s t o
    where t = if t' then "SI" else "NO"

loop1 :: [Candidato] -> Int -> [Cargo] -> [FullName] -> [Candidato]
loop1 acc _ [] _ = acc
loop1 acc idx (cat:xs) names = loop1 (catCands ++ acc) newIdx xs names
    where (catCands, newIdx) = loop2 [] cat 1 idx names

loop2 :: [Candidato] -> Cargo -> Int -> Int -> [FullName] -> ([Candidato], Int)
loop2 acc cat orden idx names
    | orden > max_ord && is_suplent = (acc, idx)
    | orden > max_ord && has_suplent = loop2 acc (carg_name, max_ord, False) 1 idx names
    | otherwise = loop2 (newCand : acc) cat (orden + 1) (idx + 1) names
    where (carg_name, max_ord, has_suplent) = cat
          is_suplent = not has_suplent
          (name, sex) = names !! (idx - 1)
          newCand = Candidato {cargo=carg_name,
                               lista=1,
                               uid=idx,
                               nombre=name,
                               orden=orden,
                               titular=is_suplent,
                               sexo=sex}
