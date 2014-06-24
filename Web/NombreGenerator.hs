{-# LANGUAGE BangPatterns #-}
module Main where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import System.Environment
import System.Exit
import Web.NombreGenerator.RandomUtil

parseArgs :: [String] -> IO Int
parseArgs ["-h"] = usage >> exit
parseArgs ["-v"] = version >> exit
parseArgs [s] = return (read s :: Int)
parseArgs [] = putStrLn "I need to know how many names you want..." >> die

usage = putStrLn "Usage: hs-nombre-generator [-h] [count ..]"
version = putStrLn "Haskell NombreGenerator 0.1"
exit = exitSuccess
die = exitWith (ExitFailure 1)

fromWeb = fromUrl "http://www.buenosaires.gob.ar/areas/registrocivil/nombres/busqueda/imprimir.php?sexo=ambos"

fromFile = readDocument [withParseHTML yes,
                         withInputEncoding isoLatin1,
                         withCheckNamespaces no,
                         withParseByMimeType no,
                         withWarnings no] "Nombres.html"

getDoc = traceMsg 0 "Downloading..." >>>
         --fromUrl url >>>
         fromFile >>>
         traceMsg 0 "Parsing..."

takeNames :: Int -> IO [String]
takeNames !count = runX findNames >>= takeRandom count

findNames :: IOSLA (XIOState ()) XmlTree String
findNames = getDoc  >>>
            css ".contenido tbody tr" >>>
            listA (
                getChildren >>>
                hasName "td"
                /> getText
            )
            >>. map head

main = getArgs >>= parseArgs >>= takeNames >>= mapM_ putStrLn
