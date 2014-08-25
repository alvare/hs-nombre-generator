module Web.NombreGenerator.Generator.Candidaturas (Cargo, FullName, candidaturas) where

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

candidaturas :: [Cargo] -> Int -> [FullName] -> [String]
candidaturas cargos listas names = map formatCand . concat $ evalState (candLoop cargos listas names) 1

candLoop :: [Cargo] -> Int -> [FullName] -> State Int [[Candidato]]
candLoop cargos listas names = forM [1..listas] (\ln -> do
                                        cands <- forM cargos (genCand names ln)
                                        return $ concat cands)

genCand :: [FullName] -> Int -> Cargo -> State Int [Candidato]
genCand names ln carg = forM titles (\title -> do
                                        idx <- get
                                        let cand = newCand title idx
                                        modify (+1)
                                        return cand)
    where (carg_name, max_ord, has_suplent) = carg
          titles = genTitle True ++ if has_suplent then genTitle False else []
          genTitle b = zip [1..max_ord] (replicate max_ord b)
          newCand (ord, titular') idx = let (name, sex) = names !! (idx - 1)
                                        in Candidato {cargo=carg_name,
                                                      lista=ln,
                                                      uid=idx,
                                                      nombre=name,
                                                      orden=ord,
                                                      titular=titular',
                                                      sexo=sex}

formatCand :: Candidato -> String
formatCand (Candidato i l n c s t' o) = printf candidato_format_csv i l n c s t o
    where t = if t' then "SI" else "NO"

candidato_format_csv :: [Char]
candidato_format_csv = "%03d,%03d,%s,,,EJ,\"%s\",%s,%s,,NO,SI,%d"
