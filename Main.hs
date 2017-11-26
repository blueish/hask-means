import System.Random
import Data.List.Split

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
    -- update the mean
    -- recur until the update of the mean matches the new update
    -- return