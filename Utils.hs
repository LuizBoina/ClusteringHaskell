import Data.List (sortBy)
{-module Utils
(
kmeans,
initCluster,
calcSSE
) where
-}

--calcSSE clss

--input: k, list of points, limit and cluster
--output: clss
kmeans k pss limit clsss
                | limit == 4 {-|| (clsss == (nextCluster pss clsss))-} = clsss
                | limit == 1 = kmeans k pss (limit+1) (frstCluster k pss)
                | otherwise = kmeans k pss (limit+1) (nextCluster pss clsss)
                where frstCluster k pss = createClusters (iniKCenValue k pss [] 1) pss (initCluster k)
                      nextCluster pss clsss = createClusters (recalKCenValue clsss) pss (initCluster k)

--input: list of centroid of k groups, k, list of points and cluster
--output: clusters (range 0 to k-1)
createClusters :: (Enum a, Floating a, Ord a) => [[a]] -> [[a]] -> [[[a]]] -> [[[a]]]
createClusters _ [] clsss = clsss
createClusters kss (ps:pss) clsss = createClusters kss pss (addToCluster clsss (idxList (minDistOf ps kss [] 9999999) kss) ps 0)

--add element in the right space in the cluster 
addToCluster :: (Eq a, Num a) => [[[a]]] -> a -> [a] -> a -> [[[a]]]
addToCluster (clss:clsss) idx ps i
                        | idx == i = (clss++[ps]):clsss
                        | otherwise = clss:(addToCluster clsss idx ps (i+1))

--return index of element in the list
idxList :: (Enum a, Eq a1, Num a) => a1 -> [a1] -> a
idxList ps pss = head [idx |(xs, idx) <- zip pss [0..] , ps == xs]

--generate a list of empty lists
initCluster :: (Enum t, Num t) => t -> [[t1]]
initCluster k = [[]| a<-[1..k]]

--input: Point, list of points, empty list and 0
--output: less distant point in the list of points of Point
minDistOf :: (Ord t, Floating t) => [t] -> [[t]] -> [t] -> t -> [t]
minDistOf xs [] ms _ = ms
minDistOf xs (ys:yss) ms minEucliDist
        | eucliDist < minEucliDist = minDistOf xs yss ys eucliDist
        | otherwise = minDistOf xs yss ms minEucliDist
        where eucliDist = euclideanDist xs ys

--input: list of list of points
--output: k centroids recalculed
recalKCenValue :: Floating a => [[[a]]] -> [[a]]
recalKCenValue [] = []
recalKCenValue (clss:clsss) = (centroid clss):(recalKCenValue clsss)

--daq pra baixo testado e correto

--sortPoints to simplify small sum
sortPoints :: (Ord a, Num a) => [[a]] -> [[a]]
sortPoints xss = sortBy comparePoints xss
                where comparePoints xs ys
                        | sum xs < sum ys = LT
                        | sum xs > sum ys = GT
                        | otherwise = compareCoordinates xs ys
                        where compareCoordinates (x:xs) (y:ys)
                                | x < y = LT
                                | x > y = GT
                                | otherwise = compareCoordinates xs ys

euclideanDist :: (Floating a) => [a]->[a]->a
euclideanDist [x] [y] = (x-y)**2
euclideanDist (x:xs) (y:ys) = sqrt ((euclideanDist [x] [y]) + (euclideanDist xs ys))

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose xss = (map head xss) : transpose (map tail xss)

centroid :: (Floating a) => [[a]]->[a]
centroid uss = map (\xs -> (sum xs) / fromIntegral(length xs)) (transpose uss)

--input: Point, list of points, empty list and 0
--output: more distant point in the list of points of Point
moreDistOf :: (Ord t, Floating t) => [t] -> [[t]] -> [t] -> t -> [t]
moreDistOf xs [] ms _ = ms
moreDistOf xs (ys:yss) ms maxEucliDist
        | eucliDist > maxEucliDist = moreDistOf xs yss ys eucliDist
        | otherwise = moreDistOf xs yss ms maxEucliDist
        where eucliDist = euclideanDist xs ys

--input: k, list of points, count = 1 and empty list of points
--output: count = k, list of k inicials centroids
iniKCenValue :: (Eq a, Floating t, Num a, Ord t) => a -> [[t]] -> [[t]] -> a -> [[t]]
iniKCenValue k pss kss count
            | count == 1 = iniKCenValue k (tail pss) ((head pss):kss) (count+1)
            | count == k+1 = kss
            | otherwise = iniKCenValue k (pssLessPoint (proxCluster pss kss) pss) (kss++[proxCluster pss kss]) (count+1)
            where pssLessPoint toRm pss = [ps | ps<-pss, ps /= toRm]
                  proxCluster pss kss = moreDistOf (centroid kss) pss [] 0
