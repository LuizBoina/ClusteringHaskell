module IO
( getK,
 --getPoints
 --ofSSECls
) where

import System.IO


getK = do k <- readFile "k.txt"
          return (read k :: Int)

getPoints = do fl <- readFile "entrada.txt"
               return (toListOfPoints fl [])
               where toListOfPoints fl pss
                           | length fl == 0 = pss
                           | otherwise = toListOfPoints (tail (dropWhile (/='\n') fl)) (pss++[toPoint fl])
                           where toPoint fl = (map (\x -> read x :: Float) (words (takeWhile (/='\n') fl)))
     