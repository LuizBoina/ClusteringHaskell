import IO
import Utils

main = do k <- getK
        pss <- getPoints
        clst <- kmeans k pss
        sse <- sseCalculate clst pss 
        -- escrever sse e pontos que pertencem a cada cluster
