import Data.List (sortBy)
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
