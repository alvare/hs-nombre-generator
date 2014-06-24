module Web.NombreGenerator.Scrapper.BsAs (takeNames) where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Web.NombreGenerator.RandomUtil

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
takeNames count = runX findNames >>= takeRandom count

findNames :: IOSLA (XIOState ()) XmlTree String
findNames = getDoc  >>>
            css ".contenido tbody tr" >>>
            listA (
                getChildren >>>
                hasName "td"
                /> getText
            )
            >>. map head
