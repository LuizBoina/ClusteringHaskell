module IO
(getPoints,
writeSSE,
writeClts,
kmeansIO
) where

import System.IO
import Text.Printf
import Utils

readInt :: String -> Int
readInt = read

readFloat :: String -> Float
readFloat = read

getK :: IO Int
getK = do k <- readFile "k.txt"
          return $ readInt k

getPoints :: IO [(Integer, [Float])]
getPoints = do fl <- readFile "entrada.txt"
               return $ tuplefy $ toListOfPoints fl
               where tuplefy pss = [(idx, ps) | (idx, ps) <- zip [1..] pss]
                     toListOfPoints fl
                           | length fl == 0 = []
                           | otherwise = [toPoint fl] ++ (toListOfPoints (tail $ dropWhile (/='\n') fl))
                           where toPoint fl = map (\x -> readFloat x) (words $ takeWhile (/='\n') fl)

writeSSE clsss = writeFile "result.txt" (printf "%.4f" (calcSSE (map toPoints clsss)))

writeClts clsss = writeFile "saida.txt" (formatClts (map toIdx clsss))

formatClts [] = ""
formatClts (clss:clsss) = (formatGroup clss)++(formatClts clsss)

formatGroup clss = map replaceChar $ tail $ show clss  
                   where replaceChar c
                            | c == ',' = ' '
                            | c == ']' = '\n'
                            | otherwise = c

kmeansIO = do k <- getK
              pss <- getPoints
              return $ kmeans k pss