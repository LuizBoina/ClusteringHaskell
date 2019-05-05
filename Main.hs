import IO
import Utils

main = do k <- getK
          pss <- getPoints
          clst <- (kmeans k (sortPoints pss) 1 (initCluster k))
          print pss
          sse <- CalcSSE clst
          ofSSECls sse
          --ofClusters clst
