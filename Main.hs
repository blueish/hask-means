import System.Random
import Data.List.Split
import Data.List

-- main = do
--     putStrLn "hello world"


-- ASSUMPTION: D = 3 for all images (rgb values are 3 integer values)
dimensions :: Int
dimensions = 3

-- each RGBValue is (n = Dimension) ints between 0 and 255
type RGBValue = [Int]
-- RGBImageData is an array of RGBValues
type RGBImageData = [RGBValue]

-- each centroid is a list of D double values between 0 and 255
type Centroid = [Double]


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
testInput :: RGBImageData
testInput = [
        [0, 0, 0],
        [1, 1, 1],
        [200, 200, 200],
        [210, 210, 210]
    ]

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
-- calculateMeansAndCentroids centroids dataSet = recalculateCentroidsAndAssignments dataSet initAcc
--     where initData = map (indexOfClosestMean centroids) dataSet
--           initAcc = (True, centroids, initData)

    -- recalculateCentroidsAndAssignments :: [[Double]] -> (Bool, [[Double]] [[Int]]) -> ([[Double]], [Int])

-- recalculateCentroidsAndAssignments dataSet (hasChanged, newCentroids, assignments) keeps updating centroids and recalculating the assignments
-- of the centroids until there were no changes in the assignments
recalculateCentroidsAndAssignments :: [[Int]] -> (Bool, [[Int]], [Int]) -> ([[Int]], [Int])
recalculateCentroidsAndAssignments dataSet (False, centroids, assignments) = (centroids, assignments)
recalculateCentroidsAndAssignments dataSet (n, centroids, assignments) = recalculateCentroidsAndAssignments dataSet (wasChanged, newCentroids, newAssignments)
    where newCentroids = recalculateCentroids assignments dataSet 
          (wasChanged, newAssignments) = updateAssignmentsFlagged newCentroids dataSet

-- recalculateCentroids assignments dataSet = 
--     map averageOfPoints
--     . map removeGroupingLabels
--     . groupBy (\a b -> (fst a) == (fst b)) 
--     . zip assignments dataSet

updateAssignmentsFlagged :: [[Int]] -> [[Int]] -> (Bool, [Int])
updateAssignmentsFlagged _ _ = (False, [ 0, 0, 1, 1])

recalculateCentroids assignments dataSet = result
    where zips = zip assignments dataSet
          groups = groupBy (\a b -> (fst a) == (fst b)) zips
          groupsNoLabels = map removeGroupingLabels groups
          result = map averageOfPoints groupsNoLabels

indexOfClosestMean :: (Floating f, Ord f) => [[ f ]] -> [ f ] -> Int
indexOfClosestMean centroids rgb = unbox $ elemIndex minDist distances
    where distances = map (distance rgb) centroids
          minDist = minimum distances
          unbox (Just x) = x
          unbox (Nothing) = error "Failed to find the index of an element in its own array."


-- removeGroupingLabels removes the tuples added by the zipping of groups
removeGroupingLabels :: [(Int, [Int])] -> [[Int]]
removeGroupingLabels arr = map groupingHelper arr
    where groupingHelper (_, a) = a

-- -- average of n points with dimensions d into one point with dimension d
averageOfPoints :: (Foldable f) => f [ Int ] -> [ Int ]
averageOfPoints arr = divVec (foldl addVec (take dimensions $ repeat 0) arr) (length arr)

addVec :: Num a => [a] -> [a] -> [a]
addVec a1 a2 = addHelper a1 a2 (length a1) []
    where addHelper a1 a2 0 acc = acc
          addHelper a1 a2 n acc = addHelper a1 a2 newN (newVal:acc)
           where newN = n - 1
                 aIdx = a1 !! newN
                 a2Idx = a2 !! newN
                 newVal = aIdx + a2Idx

divVec :: Integral f => [f] -> f -> [f]
divVec vec n = map (\a -> a `div` n) vec


-- distance :: Floating f => [ f ] -> [f] -> f
distance point1 point2 = sqrt . foldl (\acc (a, b) -> acc + ((a - b) ^ 2)) 0 $ zip point1 point2