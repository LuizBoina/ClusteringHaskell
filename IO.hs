module IO
( getK,
getPoints,
writeSse,
writeClusters
) where

import System.IO
import Text.Printf

readInt :: String -> Int
readInt = read

readFloat :: String -> Float
readFloat = read

getK = do k <- readFile "k.txt"
          return $ readInt k


getPoints = do fl <- readFile "entrada.txt"
               return $ toListOfPoints fl
               where toListOfPoints fl
                           | length fl == 0 = []
                           | otherwise = [toPoint fl] ++ (toListOfPoints (tail $ dropWhile (/='\n') fl))
                           where toPoint fl = map (\x -> readFloat x) (words $ takeWhile (/='\n') fl)

writeSse sse = writeFile "result.txt" (printf "%.4f" sse)

writeClts clsss pss = writeFile "saida.txt" (formatClts clsss pss)

formatClts [] _ = ""
formatClts (clss:clsss) pss = [formatGroup clss pss]++(formatClts clsss pss)

formatGroup (cls:clss) (ps:pss) =