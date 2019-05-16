module Utils
( kmeans,
initCluster,
calcSSE,
toPoints,
toIdx
) where

import Data.List (sortBy)

--return only points of a list of tuples
toPoints :: [(a, t)] -> [t]
toPoints xss = [snd xs | xs <- xss]

--return only index of a list of tuples
toIdx :: [(a, t)] -> [a]
toIdx xss = [fst xs | xs <- xss]

--calculate sse
calcSSE :: [[[Double]]] -> Double
calcSSE [] = 0
calcSSE (clss:clsss) = sseGroup clss (centroid clss) + calcSSE clsss
                       where sseGroup [] _ = 0
                             sseGroup (cls:clss) centroidClss = (euclideanDist cls centroidClss)**2 + sseGroup clss centroidClss
                             

--calculate initial cluster and then call recalculateGroups
--input: k and list of tuples
kmeans :: (Enum t, Eq a, Eq t, Num t) => t -> [(a, [Double])] -> [[(a, [Double])]]
kmeans k pss = recalculateGroups k pss 2 (createClusters (firstKCentroids k (sortPoints $ toPoints pss)) pss (initCluster k)) 


--input: k, list of tuples, limit and cluster
--output: clss
recalculateGroups :: (Enum t, Eq a, Eq a1, Num a, Num t) => t -> [(a1, [Double])] -> a -> [[(a1, [Double])]] -> [[(a1, [Double])]]
recalculateGroups k pss limit clsss
                | limit == 100 || clsss == (nextCluster pss clsss k) = clsss
                | otherwise = recalculateGroups k pss (limit+1) (nextCluster pss clsss k)
                where nextCluster pss clsss k = createClusters (recalKCenValue (map toPoints clsss)) pss (initCluster k)


--input: list of centroids, list of tuples and cluster
--output: clusters (range 0 to k-1)
createClusters :: [[Double]] -> [(a1, [Double])] -> [[(a1, [Double])]] -> [[(a1, [Double])]]
createClusters _ [] clsss = clsss
createClusters kss (ps:pss) clsss = createClusters kss pss (addToCluster clsss (idxList (minDistOf (snd ps) kss [] 9999999) kss) ps 0)

--add element in the right local in the cluster
addToCluster :: (Eq a, Num a) => [[a1]] -> a -> a1 -> a -> [[a1]]
addToCluster (clss:clsss) idx ps i
                        | idx == i = (clss++[ps]):clsss
                        | otherwise = clss:(addToCluster clsss idx ps (i+1))

--return index of element in the list
idxList :: (Enum a, Eq a1, Num a) => a1 -> [a1] -> a
idxList ps pss = head [idx |(xs, idx) <- zip pss [0..] , ps == xs]

--generate a empty lists
initCluster :: (Enum t, Num t) => t -> [[t1]]
initCluster k = [[]| a<-[1..k]]

--input: Point, list of points, empty list and 0
--output: less distant point in the list of points of Point
minDistOf :: [Double] -> [[Double]] -> [Double] -> Double -> [Double]
minDistOf xs [] ms _ = ms
minDistOf xs (ys:yss) ms minEucliDist
        | eucliDist < minEucliDist = minDistOf xs yss ys eucliDist
        | otherwise = minDistOf xs yss ms minEucliDist
        where eucliDist = euclideanDist xs ys

--input: list of list of points
--output: k centroids recalculed
recalKCenValue :: [[[Double]]] -> [[Double]]
recalKCenValue [] = []
recalKCenValue (clss:clsss) = (centroid clss):(recalKCenValue clsss)


--sortPoints to simplify small sum
sortPoints :: (Num a, Ord a, Ord (t a), Foldable t) => [t a] -> [t a]
sortPoints xss = sortBy comparePoints xss
                where comparePoints xs ys
                        | sum xs < sum ys = LT
                        | sum xs > sum ys = GT
                        | otherwise = compare xs ys

transpose :: [[b]] -> [[b]]
transpose ([]:_) = []
transpose xss = (map head xss) : transpose (map tail xss)

euclideanDist :: Floating a => [a] -> [a] -> a
euclideanDist xs ys = sqrt (dist xs ys)
              where dist (p:ps) (q:qs) = (q - p)**2 + (dist ps qs)
                    dist [] [] = 0


centroid :: Fractional a => [[a]] -> [a]
centroid uss = map (\xs -> (sum xs) / fromIntegral(length xs)) (transpose uss)

--input: Point, list of points, empty list and 0
--output: more distant point in the list of points of Point
moreDistOf :: (Floating a, Ord a) => [a] -> [[a]] -> [a] -> a -> [a]
moreDistOf xs [] ms _ = ms
moreDistOf xs (ys:yss) ms maxEucliDist
        | eucliDist > maxEucliDist = moreDistOf xs yss ys eucliDist
        | otherwise = moreDistOf xs yss ms maxEucliDist
        where eucliDist = euclideanDist xs ys

--get the first point of sorted list by sum of the points to by 
--the first point of centroid and then call restOfFirstKCentroids
--to calculate the other values of first centroid
firstKCentroids:: (Eq a, Floating a1, Num a, Ord a1) => a -> [[a1]] -> [[a1]]
firstKCentroids k pss = (restOfFirstKCentroids (k-1) (tail pss) [(head pss)])

restOfFirstKCentroids :: (Eq a, Floating a1, Num a, Ord a1) => a -> [[a1]] -> [[a1]] -> [[a1]]
restOfFirstKCentroids k pss kss
            | k == 0 = kss
            | otherwise = restOfFirstKCentroids (k-1) (pssLessPoint (proxCluster pss kss) pss) (kss++[proxCluster pss kss])
            where pssLessPoint toRm pss = [ps | ps<-pss, ps /= toRm]
                  proxCluster pss kss = moreDistOf (centroid kss) pss [] 0