module Utils
( kmeans,
initCluster,
calcSSE,
toPoints,
toIdx
) where

import Data.List (sortBy)

toPoints xss = [snd xs | xs <- xss]
toIdx xss = [fst xs | xs <- xss]

calcSSE [] = 0
calcSSE (clss:clsss) = sseGroup clss (centroid clss) + calcSSE clsss
                       where sseGroup [] _ = 0
                             sseGroup (cls:clss) centroidClss = (euclideanDist cls centroidClss)**2 + sseGroup clss centroidClss
                             

--primeira iteracao ja ocorre na chamada da funcao
kmeans k pss = recalculateGroups k pss 2 (createClusters (firstKCentroids k (sortPoints $ toPoints pss)) pss (initCluster k)) 


--input: k, list of points, limit and cluster
--output: clss
--recalculateGroups :: (Floating a1, Ord a1, Enum a1, Enum t, Eq a2, Eq t, Num a2, Num t) => t -> [[a1]] -> a2 -> [[[a1]]] -> [[[a1]]]
recalculateGroups k pss limit clsss
                | limit == 100 || clsss == (nextCluster pss clsss k) = clsss
                | otherwise = recalculateGroups k pss (limit+1) (nextCluster pss clsss k)
                where nextCluster pss clsss k = createClusters (recalKCenValue (map toPoints clsss)) pss (initCluster k)


--input: list of centroid of k groups, k, list of points and cluster
--output: clusters (range 0 to k-1)
createClusters _ [] clsss = clsss
createClusters kss (ps:pss) clsss = createClusters kss pss (addToCluster clsss (idxList (minDistOf (snd ps) kss [] 9999999) kss) ps 0)

--add element in the right space in the cluster 
addToCluster (clss:clsss) idx ps i
                        | idx == i = (clss++[ps]):clsss
                        | otherwise = clss:(addToCluster clsss idx ps (i+1))

--return index of element in the list
idxList ps pss = head [idx |(xs, idx) <- zip pss [0..] , ps == xs]

--generate a list of empty lists
initCluster k = [[]| a<-[1..k]]

--input: Point, list of points, empty list and 0
--output: less distant point in the list of points of Point
minDistOf xs [] ms _ = ms
minDistOf xs (ys:yss) ms minEucliDist
        | eucliDist < minEucliDist = minDistOf xs yss ys eucliDist
        | otherwise = minDistOf xs yss ms minEucliDist
        where eucliDist = euclideanDist xs ys

--input: list of list of points
--output: k centroids recalculed
recalKCenValue [] = []
recalKCenValue (clss:clsss) = (centroid clss):(recalKCenValue clsss)


--sortPoints to simplify small sum
sortPoints xss = sortBy comparePoints xss
                where comparePoints xs ys
                        | sum xs < sum ys = LT
                        | sum xs > sum ys = GT
                        | otherwise = compare xs ys

transpose ([]:_) = []
transpose xss = (map head xss) : transpose (map tail xss)

euclideanDist xs ys = sqrt (dist xs ys)
              where dist (p:ps) (q:qs) = (q - p)**2 + (dist ps qs)
                    dist [] [] = 0



centroid uss = map (\xs -> (sum xs) / fromIntegral(length xs)) (transpose uss)

--input: Point, list of points, empty list and 0
--output: more distant point in the list of points of Point
moreDistOf xs [] ms _ = ms
moreDistOf xs (ys:yss) ms maxEucliDist
        | eucliDist > maxEucliDist = moreDistOf xs yss ys eucliDist
        | otherwise = moreDistOf xs yss ms maxEucliDist
        where eucliDist = euclideanDist xs ys

firstKCentroids k pss = (restOfFirstKCentroids (k-1) (tail pss) [(head pss)])

restOfFirstKCentroids k pss kss
            | k == 0 = kss
            | otherwise = restOfFirstKCentroids (k-1) (pssLessPoint (proxCluster pss kss) pss) (kss++[proxCluster pss kss])
            where pssLessPoint toRm pss = [ps | ps<-pss, ps /= toRm]
                  proxCluster pss kss = moreDistOf (centroid kss) pss [] 0