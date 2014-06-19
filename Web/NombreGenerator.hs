module Main where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import System.Environment
import System.Exit

parse :: [String] -> IO Int
parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse [s] = return (read s :: Int)
parse [] = needCount >> die

usage = putStrLn "Usage: hs-nombre-generator [-h] [count ..]"
version = putStrLn "Haskell NombreGenerator 0.1"
needCount = putStrLn "I need to know how many names you want..."
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)

doc = fromUrl "http://www.buenosaires.gob.ar/areas/registrocivil/nombres/busqueda/imprimir.php?sexo=ambos"

magic count = do
    names <- runX $ doc >>> css ".contenido tbody tr" >>> listA (getChildren >>> hasName "td" /> getText) >>. map head
    mapM_ putStrLn $ take count names

main = getArgs >>= parse >>= magic
