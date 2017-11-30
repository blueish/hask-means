import System.Random
import Data.List.Split
import Data.List
import Data.Vector.Storable (toList)
import Codec.Picture
import GHC.Word
import qualified Data.Map as Map
import Debug.Trace

--main will read an image from filepath str and return [[RGB values]]
-- main :: [Char] -> IO [[Word8]]
imgFromPath path = do
    img <- readImage path
    let rgb8 = convertRGB8 $ ignoreError img
    let pixels = chunksOf 3 $ map fromIntegral $ map toInteger $ toList (imageData rgb8)
    return pixels

-- ASSUMPTION: D = 3 for all images (rgb values are 3 integer values)
dimensions :: Int
dimensions = 3

-- each RGBValue is (n = Dimension) ints between 0 and 255
type RGBValue = [Int]
-- RGBImageData is an array of RGBValues
type RGBImageData = [RGBValue]

-- each mean is a list of D double values between 0 and 255
type Mean = [Double]

-- a list with a 1:1 map representing a mapping of each RGBValue to each Mean
type MeanAssignments = [Int]

-- new assignments
type NewMeanAssignments = Map.Map Mean [RGBValue]


ignoreError :: Either t b -> b
ignoreError (Left a) = error "merp"
ignoreError (Right a) = a

-- quantizeImage path bits = do
--     img <- imgFromPath path
--     (means, labels) <- kmeans (2^bits) img
--     return (means, labels)
    -- return 0
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
        -- Future work: change this to use kmeans++ to calculate initial centroids
        rg <- newStdGen
        let initialMeans = chunksOf dimensions
                . take (dimensions * k)
                $ (randomRs (0, 255) rg :: [Double])

        print "The initial means: "
        print initialMeans
        print "length of dataaset"
        print (length dataSet)
        -- assign each object to initial closest mean
        -- let closestMeans = map (indexOfClosestMean initialMeans) dataSet
        let newAssigns = calculateMeanMap initialMeans dataSet

        -- let finalMeans = calculateMeansAndAssignments initialMeans dataSet
        print "Final means: "
        -- return finalMeans
        return 0


calculateMeanMap :: [Mean] -> [RGBValue] -> NewMeanAssignments
calculateMeanMap means dataSet = Map.union
    (Map.fromList . zip means $ groupBy (\a b -> indexOfClosestMean means a == indexOfClosestMean means b) dataSet)
    $ Map.fromList [ (m, []) | m <- means ]

calculateAssignments :: NewMeanAssignments -> RGBImageData -> NewMeanAssignments
calculateAssignments meanmap dataset = recalculateAssignments meanmap dataset True 


recalculateAssignments :: NewMeanAssignments -> RGBImageData -> Bool -> NewMeanAssignments
recalculateAssignments meanmap _ False = meanmap
recalculateAssignments meanmap dataset _ = recalculateAssignments newmap dataset waschanged
        where newmeans = createNewMeans (trace ("old means: " ++ show meanmap) meanmap)
              (waschanged, newmap) = updateAssignments newmeans dataset

createNewMeans :: NewMeanAssignments -> [Mean]
createNewMeans meanmap = Map.foldrWithKey
    (\key val acc -> if null val
                        then key:acc -- if the list is empty, don't modify this mean
                        else (calculateNewMean val):acc) -- else generate the new mean from the centroid of values
    []
    meanmap

updateAssignments :: [Mean] -> RGBImageData -> (Bool, NewMeanAssignments)
updateAssignments means dataset = (waschanged, newMeanMap)
    where newMeanMap = calculateMeanMap means dataset
          waschanged = not $ allEqual means $ Map.keys newMeanMap


{-
-- calculateMeansAndAssignments means dataSet =  takes initial means and a dataSet, and recalculates the means
-- until an update of the means lead to no changes, returning the final means and the group assignment values for the dataSet
calculateMeansAndAssignments :: [Mean] -> RGBImageData -> ([Mean], MeanAssignments)
calculateMeansAndAssignments means dataSet = recalculateMeansAndAssignments dataSet True means initData
    where initData = map (indexOfClosestMean means) dataSet


-- recalculateMeansAndAssignments dataSet hasChanged newMeans assignments keeps updating means and recalculating the assignments
-- of the means until there were no changes in the assignments
recalculateMeansAndAssignments :: RGBImageData -> Bool -> [Mean] -> MeanAssignments -> ([Mean], MeanAssignments)
recalculateMeansAndAssignments dataSet False means assignments = (means, assignments)
recalculateMeansAndAssignments dataSet _     means assignments = recalculateMeansAndAssignments
    dataSet wasChanged newMeans newAssignments
        where newMeans = recalculateMeans (trace ("new means: " ++ show means) means) assignments dataSet
              (wasChanged, newAssignments) = updateAssignmentsFlagged newMeans dataSet assignments
-}

-- takes means and a dataset, and gives back whether any changed alongside the new mappings
updateAssignmentsFlagged :: [Mean] -> RGBImageData -> MeanAssignments -> (Bool, MeanAssignments)
updateAssignmentsFlagged means dataSet oldAssignments = (not $ allEqual oldAssignments newAssignments, newAssignments)
    where newAssignments = map (indexOfClosestMean means) dataSet


emptyArrayMap :: Int -> Map.Map Int [a]
emptyArrayMap n = Map.fromList [ (i, []) | i <- [0..(n - 1)] ]

-- todo: future refactor should make mean assignments not use indicies. Maybe a map?
-- iterating over the indicies and using that index feels like it's the wrong way
recalculateMeans :: [Mean] -> MeanAssignments -> RGBImageData -> [Mean]
recalculateMeans oldMeans assignments dataSet = map mapIndex [0..(length oldMeans - 1)] 
    where oldAssignmentMap = generateAssignedVectorMap oldMeans assignments dataSet
          mapIndex = (\idx -> let assignedValues = oldAssignmentMap Map.! idx
           in if (null assignedValues)
                then oldMeans !! idx
                else calculateNewMean $ oldAssignmentMap Map.! idx)

-- generateAssignedVectorMap :: [Mean] -> MeanAssignments -> RGBImageData -> [Mean]
generateAssignedVectorMap oldMeans assignments dataSet =
    foldl insertGroupIntoMap (emptyArrayMap $ length oldMeans - 1) -- :: Map Int [ RGBValue ]
        $ zip assignments dataSet -- ::[ (Int, RGBValue) ]
    where insertGroupIntoMap = \accMap (idx, rgbvector) -> Map.insertWith (++) idx [rgbvector] accMap
        

-- returns the index of the closest mean
-- used for assignment of the dataset to means
indexOfClosestMean :: [Mean] -> RGBValue -> Int
indexOfClosestMean means rgb = unbox $ elemIndex minDist distances
    where distances = map (distance rgb) means
          minDist = minimum distances
          unbox (Just x) = x
          unbox (Nothing) = error "Failed to find the index of an element in its own array."

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
