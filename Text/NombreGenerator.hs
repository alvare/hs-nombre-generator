import Text.XML.HXT.Core
import Text.HandsomeSoup
import System.Environment

doc = fromUrl "http://www.buenosaires.gob.ar/areas/registrocivil/nombres/busqueda/imprimir.php?sexo=ambos"

main = do
    args <- getArgs
    let cant = read (head args) :: Int
    file <- readFile "Nombres.html"
    links <- runX $ doc >>> css ".contenido tbody tr" >>> listA (getChildren >>> hasName "td" /> getText) >>. map head
    mapM_ putStrLn $ take cant links
