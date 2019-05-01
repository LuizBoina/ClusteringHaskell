import IO
import Utils

main = do k <- getK
        pss <- getPoints
        createClusters k pss
        putSSE 
        writeFile "saida.txt"
