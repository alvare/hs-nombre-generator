{-# LANGUAGE BangPatterns #-}
module Main where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import System.Environment
import System.Exit

parseArgs :: [String] -> IO Int
parseArgs ["-h"] = usage >> exit
parseArgs ["-v"] = version >> exit
parseArgs [s] = return (read s :: Int)
parseArgs [] = putStrLn "I need to know how many names you want..." >> die

usage = putStrLn "Usage: hs-nombre-generator [-h] [count ..]"
version = putStrLn "Haskell NombreGenerator 0.1"
exit = exitSuccess
die = exitWith (ExitFailure 1)

--url = "http://www.buenosaires.gob.ar/areas/registrocivil/nombres/busqueda/imprimir.php?sexo=ambos"

lala url = readDocument [withParseHTML yes,
                         withInputEncoding isoLatin1,
                         withCheckNamespaces no,
                         withParseByMimeType no
                         --withTagSoup
                         --withWarnings no,
                         ]
                         url

doc = traceMsg 0 "Reading..." >>> lala "Nombres.html" >>> traceMsg 0 "Parsed"

magic !count = do
    names <- runX $ doc  >>> css ".contenido tbody tr" >>> listA (getChildren >>> hasName "td" /> getText) >>. map head
    mapM_ putStrLn $ take count names

main = getArgs >>= parseArgs >>= magic
