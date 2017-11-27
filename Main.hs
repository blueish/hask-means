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

-- a list with a 1:1 map representing a mapping of each RGBValue to each Centroid
type CentroidAssignments = [Int]

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
        let closestMeans = map (indexOfClosestMean initialCentroids) dataSet
        print "initial closest means: "
        print closestMeans

        let finalMeans = calculateMeansAndCentroids initialCentroids dataSet
        print "returning..."
        return finalMeans

-- calculateMeansAndCentroids centroids dataSet =  takes initial centroids and a dataSet, and recalculates the centroids
-- until an update of the centroids lead to no changes, returning the final centroids and the group assignment values for the dataSet
calculateMeansAndCentroids :: [Centroid] -> RGBImageData -> ([Centroid], CentroidAssignments)
calculateMeansAndCentroids centroids dataSet = recalculateCentroidsAndAssignments dataSet True centroids initData
    where initData = map (indexOfClosestMean centroids) dataSet

    -- recalculateCentroidsAndAssignments :: [[Double]] -> (Bool, [[Double]] [[Int]]) -> ([[Double]], [Int])

-- recalculateCentroidsAndAssignments dataSet hasChanged newCentroids assignments keeps updating centroids and recalculating the assignments
-- of the centroids until there were no changes in the assignments
recalculateCentroidsAndAssignments :: RGBImageData -> Bool -> [Centroid] -> CentroidAssignments -> ([Centroid], CentroidAssignments)
recalculateCentroidsAndAssignments dataSet False centroids assignments = (centroids, assignments)
recalculateCentroidsAndAssignments dataSet _     centroids assignments = recalculateCentroidsAndAssignments
    dataSet wasChanged newCentroids newAssignments
        where newCentroids = recalculateCentroids assignments dataSet 
              (wasChanged, newAssignments) = updateAssignmentsFlagged newCentroids dataSet assignments


-- takes centroids and a dataset, and gives back whether any changed alongside the new mappings
updateAssignmentsFlagged :: [Centroid] -> RGBImageData -> CentroidAssignments -> (Bool, CentroidAssignments)
updateAssignmentsFlagged centroids dataSet oldAssignments = (allEqual oldAssignments newAssignments, newAssignments)
    where newAssignments = map (indexOfClosestMean centroids) dataSet




-- given the current centroid assignments and the dataset, calculates the new means
recalculateCentroids :: CentroidAssignments -> RGBImageData -> [Centroid]
recalculateCentroids assignments dataSet = 
    map calculateNewMean
    . map removeGroupingLabels
    . groupBy (\a b -> (fst a) == (fst b)) 
    $ zip assignments dataSet


-- returns the index of the closest centroid (mean)
-- used for assignment of the dataset to centroids
indexOfClosestMean :: [Centroid] -> RGBValue -> Int
indexOfClosestMean centroids rgb = unbox $ elemIndex minDist distances
    where distances = map (distance rgb) centroids
          minDist = minimum distances
          unbox (Just x) = x
          unbox (Nothing) = error "Failed to find the index of an element in its own array."


-- removeGroupingLabels removes the tuples added by the zipping of groups
removeGroupingLabels :: [(a, b)] -> [b]
removeGroupingLabels arr = map snd arr

-- average of n points with dimensions d into one point with dimension d
calculateNewMean :: [RGBValue] -> Centroid
calculateNewMean arr = divVec (foldl addVec initialZeroVector arr) (length arr)
    where initialZeroVector = take dimensions $ repeat 0

-- TODO: this is gross, there has to be a better way to fold two arrays
addVec :: [Int] -> [Int] -> [Int]
addVec a1 a2 = addHelper a1 a2 (length a1) []
    where addHelper a1 a2 0 acc = acc
          addHelper a1 a2 n acc = addHelper a1 a2 newN (newVal:acc)
           where newN = n - 1
                 aIdx = a1 !! newN
                 a2Idx = a2 !! newN
                 newVal = aIdx + a2Idx

allEqual :: Eq a => [a] -> [a] -> Bool
allEqual c1 c2 = and $ zipWith (==) c1 c2

-- Divides each value of vec by n
divVec :: [Int] -> Int -> Centroid
divVec vec n = map (\a -> a / (fromIntegral n)) $ map fromIntegral vec


distance :: RGBValue -> Centroid -> Double
distance rgb cent = sqrt 
    . foldl (\acc (a, b) -> acc + ((a - b) ^ 2)) 0
    -- have to map each Int so we can add them to doubles
    $ zip (map fromIntegral rgb) cent