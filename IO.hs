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

getPoints :: IO [[Float]]
getPoints = do fl <- readFile "entrada.txt"
               return $ toListOfPoints fl
               where toListOfPoints fl
                           | length fl == 0 = []
                           | otherwise = [toPoint fl] ++ (toListOfPoints (tail $ dropWhile (/='\n') fl))
                           where toPoint fl = map (\x -> readFloat x) (words $ takeWhile (/='\n') fl)

writeSSE :: (PrintfArg t, Floating t) => [[[t]]] -> IO ()
writeSSE clsss = writeFile "result.txt" (printf "%.4f" (calcSSE clsss))

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

kmeansIO :: IO [[[Float]]]
kmeansIO = do k <- getK
              pss <- getPoints
              return $ kmeans k pss