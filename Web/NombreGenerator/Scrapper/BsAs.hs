module Web.NombreGenerator.Scrapper.BsAs (Name, scrap) where

import Text.XML.HXT.Core
import Text.HandsomeSoup

type Name = (String, String)

fromWeb = fromUrl "http://www.buenosaires.gob.ar/areas/registrocivil/nombres/busqueda/imprimir.php?sexo=ambos"

fromFile = readDocument [withParseHTML yes,
                         withInputEncoding isoLatin1,
                         withCheckNamespaces no,
                         withParseByMimeType no,
                         withWarnings no] "Nombres.html"

getDoc = traceMsg 0 "Downloading..." >>>
         --fromWeb >>>
         fromFile >>>
         traceMsg 0 "Parsing..."

scrap :: IO [Name]
scrap = runX findNames

findNames :: IOSLA (XIOState ()) XmlTree Name
findNames = getDoc  >>>
            css ".contenido tbody tr" >>>
            listA (
                getChildren >>>
                hasName "td"
                /> getText
            )
            >>. map takeTwo

takeTwo :: [a] -> (a, a)
takeTwo (x:y:_) = (x, y)
