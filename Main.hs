import System.Random
import Data.List.Split
import Data.List

-- main = do
--     putStrLn "hello world"


-- ASSUMPTION: D = 3 for all images (rgb values are 3 integer values)
dimensions :: Int
dimensions = 2

-- each RGBValue is (n = Dimension) ints between 0 and 255
type RGBValue = [Int]
-- RGBImageData is an array of RGBValues
type RGBImageData = [RGBValue]

-- each mean is a list of D double values between 0 and 255
type Mean = [Double]

-- a list with a 1:1 map representing a mapping of each RGBValue to each Mean
type MeanAssignments = [Int]

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
        -- initialize means (using Random)
        -- our values are RGB values, so we'll take number pairs from 0 to 255
        rg <- newStdGen
        let initialMeans = chunksOf dimensions
                . take (dimensions * k)
                $ (randomRs (0, 255) rg :: [Double])

        print "the initial means: "
        print initialMeans
        -- assign each object to initial closest mean
        let closestMeans = map (indexOfClosestMean initialMeans) dataSet
        print "initial closest means: "
        print closestMeans

        let finalMeans = calculateMeansAndMeans initialMeans dataSet
        print "returning..."
        return finalMeans

-- calculateMeansAndMeans means dataSet =  takes initial means and a dataSet, and recalculates the means
-- until an update of the means lead to no changes, returning the final means and the group assignment values for the dataSet
calculateMeansAndMeans :: [Mean] -> RGBImageData -> ([Mean], MeanAssignments)
calculateMeansAndMeans means dataSet = recalculateMeansAndAssignments dataSet True means initData
    where initData = map (indexOfClosestMean means) dataSet


-- recalculateMeansAndAssignments dataSet hasChanged newMeans assignments keeps updating means and recalculating the assignments
-- of the means until there were no changes in the assignments
recalculateMeansAndAssignments :: RGBImageData -> Bool -> [Mean] -> MeanAssignments -> ([Mean], MeanAssignments)
recalculateMeansAndAssignments dataSet False means assignments = (means, assignments)
recalculateMeansAndAssignments dataSet _     means assignments = recalculateMeansAndAssignments
    dataSet wasChanged newMeans newAssignments
        where newMeans = recalculateMeans assignments dataSet 
              (wasChanged, newAssignments) = updateAssignmentsFlagged newMeans dataSet assignments


-- takes means and a dataset, and gives back whether any changed alongside the new mappings
updateAssignmentsFlagged :: [Mean] -> RGBImageData -> MeanAssignments -> (Bool, MeanAssignments)
updateAssignmentsFlagged means dataSet oldAssignments = (allEqual oldAssignments newAssignments, newAssignments)
    where newAssignments = map (indexOfClosestMean means) dataSet




-- given the current mean assignments and the dataset, calculates the new means
recalculateMeans :: MeanAssignments -> RGBImageData -> [Mean]
recalculateMeans assignments dataSet = 
    map calculateNewMean
    . map removeGroupingLabels
    . groupBy (\a b -> (fst a) == (fst b)) 
    $ zip assignments dataSet


-- returns the index of the closest mean
-- used for assignment of the dataset to means
indexOfClosestMean :: [Mean] -> RGBValue -> Int
indexOfClosestMean means rgb = unbox $ elemIndex minDist distances
    where distances = map (distance rgb) means
          minDist = minimum distances
          unbox (Just x) = x
          unbox (Nothing) = error "Failed to find the index of an element in its own array."


-- removeGroupingLabels removes the tuples added by the zipping of groups
removeGroupingLabels :: [(a, b)] -> [b]
removeGroupingLabels arr = map snd arr

-- average of n points with dimensions d into one point with dimension d
calculateNewMean :: [RGBValue] -> Mean
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
divVec :: [Int] -> Int -> Mean
divVec vec n = map (\a -> a / (fromIntegral n)) $ map fromIntegral vec


distance :: RGBValue -> Mean -> Double
distance rgb cent = sqrt 
    . foldl (\acc (a, b) -> acc + ((a - b) ^ 2)) 0
    -- have to map each Int so we can add them to doubles
    $ zip (map fromIntegral rgb) cent