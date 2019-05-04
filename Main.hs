import IO
import Utils

main = do k <- getK
          pss <- getPss
          clst <- (kmeans k (sortPoints pss))
          sse <- CalcSSE clst pss 
          -- escrever sse e pontos que pertencem a cada cluster
