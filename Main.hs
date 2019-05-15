import IO

main = do clst <- kmeansIO
          pss <- getPoints
          writeSSE clst
          writeClts clst