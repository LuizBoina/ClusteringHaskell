import Data.List (sortBy)
{-module Utils
(
kmeans
) where
-}

kmeans k pss = createClusters (iniClusters k pss [] 1) k pss

createClusters iniCls k pss

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
--output: count = k, list of inicials centroids
iniClusters k pss kss count
            | count == 1 = iniClusters k (tail pss) ((head pss):kss) (count+1)
            | count == k+1 = kss
            | otherwise = iniClusters k (pssLessPoint (proxCluster pss kss) pss) (kss++[proxCluster pss kss]) (count+1)
            where pssLessPoint toRm pss = [ps | ps<-pss, ps /= toRm]
                  proxCluster pss kss = moreDistOf (centroid kss) pss [] 0