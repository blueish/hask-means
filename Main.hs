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
testInput :: [[Double]]
testInput = [
        [0, 0, 0],
        [1, 1, 1],
        [200, 200, 200],
        [210, 210, 210]
    ]

-- ASSUMPTION: D = 3 for all images (rgb values are 3 integer values)
dimensions = 3

-- kmeans :: Fractional f =>Int -> [[ Int ]] -> ([[ f ]], [ Int ])
kmeans k dataSet = do
        -- initialize centroids (using Random)
        -- our values are RGB values, so we'll take number pairs from 0 to 255
        rg <- newStdGen
        let initialCentroids = chunksOf dimensions
                . take (dimensions * k)
                $ (randomRs (0, 255) rg :: [Double])

        print "the initial centroids: "
        print initialCentroids
        -- assign each object to initial closest mean
        -- let closestMeans = map (indexOfClosestMean initialCentroids) dataSet
        -- print "initial closest means: "
        -- print closestMeans

        -- let finalMeans = calculateMeansAndCentroids initialCentroids dataSet
        -- update the means
        -- recur until the update of the mean matches the new update
        -- return
        -- print "returning..."
        -- return finalMeans
        return 0

-- calculateMeansAndCentroids centroids dataSet =  takes initial centroids and a dataSet, and recalculates the centroids
-- until an update of the centroids lead to no changes, returning the final centroids and the group assignment values for the dataSet
-- calculateMeansAndCentroids :: [[Double]] -> [[Double]] -> ([[Double]], [Int])
calculateMeansAndCentroids centroids dataSet = helper dataSet (True, centroids, initData)
    where initData = map (indexOfClosestMean centroids) dataSet
    -- helper dataSet (hasChanged, newCentroids, assignments) keeps updating centroids and recalculating the assignments
    -- of the centroids until there were no changes in the assignments
    -- helper :: [[Double]] -> (Bool, [[Double]] [[Int]]) -> ([[Double]], [Int])

helper dataSet (False, centroids, assignments) = (centroids, assignments)
helper dataSet (n, centroids, assignments) = helper dataSet (wasChanged, newCentroids, newAssignments)
    where newCentroids = recalculateCentroids dataSet assignments
          (wasChanged, newAssignments) = updateAssignmentsFlagged


-- updateAssignmentsFlagged = (False, [0, 1, 2])

-- indexOfClosestMean :: (Floating f, Ord f) => [[ f ]] -> [ f ] -> Int
indexOfClosestMean centroids rgb = unbox $ elemIndex minDist distances
    where distances = map (distance rgb) centroids
          minDist = minimum distances
          unbox (Just x) = x
          unbox (Nothing) = error "Failed to find the index of an element in its own array."

-- distance :: Floating f => [ f ] -> [f] -> f
distance point1 point2 = sqrt . foldl (\acc (a, b) -> acc + ((a - b) ^ 2)) 0 $ zip point1 point2