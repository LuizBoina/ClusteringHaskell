import System.IO
{-(readFile,
writeFile
) where
-}

getK = do x <- readFile "k.txt"
          return x

getPoints = do fl <- openFile "entrada.txt" ReadMode
               if hIsEOF fl then
                hClose fl
               else
                ps <- 
              

--writeFile fileName =
    