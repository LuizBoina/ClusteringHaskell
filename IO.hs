module IO
( getK,
getPoints,
writeSse,
writeClts
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

writeSse :: PrintfArg t => t -> IO ()
writeSse sse = writeFile "result.txt" (printf "%.4f" sse)

writeClts :: Eq a => [[[a]]] -> [[a]] -> IO ()
writeClts clsss pss = writeFile "saida.txt" (formatClts clsss pss)

formatClts :: Eq a => [[[a]]] -> [[a]] -> [Char]
formatClts [] _ = ""
formatClts (clss:clsss) pss = formatGroup clss pss++(formatClts clsss pss)

formatGroup :: Eq a => [[a]] -> [[a]] -> [Char]
formatGroup clss pss = map replaceChar $ tail $ show [idx | cls<-clss, (idx,ps)<-zip [1..] pss , cls == ps]
                       where replaceChar c
                                    | c == ',' = ' '
                                    | c == ']' = '\n'
                                    | otherwise = c