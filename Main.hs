import System.Random
import Data.List.Split
import Data.List

-- main = do
--     putStrLn "hello world"



{-
 2 -> [ [0,0], [2.1,2] [2,2]] -> 
 (
    -- this is the list of all of the means
    [
        [0,0],
        [2.05, 2]
    ],
    -- this is the assignment/labels for each element
    -- mapping the training example to mean
    [ 1, 2 2 ]
  )
-}


-- ASSUMPTION: D = 3 for all images
dimensions = 3

-- kmeans :: Fractional f =>Int -> [[ Int ]] -> ([[ f ]], [ Int ])
kmeans k x = do
        -- initialize centroids (using Random)
        rg <- newStdGen
        -- our values are RGB values, so we'll take number pairs
        -- from 0 to 255
        let val = chunksOf dimensions
                . map (*255)
                . take (dimensions * k) $ (randoms rg :: [Double])
        return val
        -- assign each object to closest mean
        -- update the means
        -- recur until the update of the mean matches the new update
        -- return

indexOfClosestMean :: (Floating f, Ord f) => [ f ] -> [[ f ]] -> Int
indexOfClosestMean rgb centroids = unbox $ elemIndex minDist distances
    where distances = map (distance rgb) centroids
          minDist = minimum distances
          unbox (Just x) = x
          unbox (Nothing) = error "this should never happen"

distance :: Floating f => [ f ] -> [f] -> f
distance point1 point2 = sqrt . foldl (\acc (a, b) -> acc + ((a - b) ^ 2)) 0 $ zip point1 point2