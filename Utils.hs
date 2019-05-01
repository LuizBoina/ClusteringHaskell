import Data.List (sortBy)
{-module Utils
(
createClusters
) where


createClusters k pss
-}
--a entrada sempre vai estar correta?

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

centroid :: (Floating a) => [[a]]->[a]
centroid uss = map (\xs -> (sum xs) / fromIntegral(length xs)) uss

moreDistOf :: (Ord t, Floating t) => [t] -> [[t]] -> [t] -> t -> [t]
moreDistOf xs [] ms _ = ms
moreDistOf xs (ys:yss) ms maxEucliDist
        | eucliDist > maxEucliDist = moreDistOf xs yss ys eucliDist
        | otherwise = moreDistOf xs yss ms maxEucliDist
        where eucliDist = euclideanDist xs ys

