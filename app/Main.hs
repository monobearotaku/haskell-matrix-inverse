module Main where

import System.Random (randomRIO)
import Text.Printf (printf)
import Control.Parallel.Strategies
import Control.Exception (evaluate)

type Matrix = [[Double]]

identityMatrix:: Int -> Matrix
identityMatrix n = [[if j == i then 1.0 else 0.0 | i <- [0..n-1] ] | j <- [0..n-1] ]

radnomMatrix:: Int -> IO Matrix
radnomMatrix n = sequence [randomRow n | _ <- [1..n]]

randomRow :: Int -> IO [Double]
randomRow n = sequence [randomDouble | _ <- [1..n]]

randomDouble:: IO Double
randomDouble = randomRIO (5.0, 10.0)

printMatrix:: Matrix -> IO ()
printMatrix = mapM_ printRow
  where
    printRow row = putStrLn $ concatMap (printf "%.2f ") row

inverseMatrix :: Matrix -> Matrix
inverseMatrix a =
    let n = length a
        identity = identityMatrix n
        (leftInv, _) = foldl processRow (a, identity) [0..n-1]
    in leftInv

processRow :: (Matrix, Matrix) -> Int -> (Matrix, Matrix)
processRow (left, right) idx =
    let n = length left
        pivotValue = left !! idx !! idx

        (leftScaled, rightScaled) = (scaleRow (1 / pivotValue) idx left,
                                     scaleRow (1 / pivotValue) idx right)

        eliminate k (l, r) = if k == idx then (l, r) else
            let factor = l !! k !! idx
            in (subtractRow factor idx k l, subtractRow factor idx k r)

    in foldr eliminate (leftScaled, rightScaled) [0..n-1] 

scaleRow :: Double -> Int -> Matrix -> Matrix
scaleRow factor idx mat = take idx mat ++ [parMap r0 (* factor) (mat !! idx)] ++ drop (idx + 1) mat

subtractRow :: Double -> Int -> Int -> Matrix -> Matrix
subtractRow factor srcIdx destIdx mat =
    let srcRow = parMap r0 (* factor) (mat !! srcIdx)
    in take destIdx mat ++ [zipWith (-) (mat !! destIdx) srcRow] ++ drop (destIdx + 1) mat

main :: IO ()
main = do
    let n = 500
    a <- radnomMatrix n
    -- putStrLn "Original Matrix:"
    -- printMatrix a
    -- putStrLn "Inverse Matrix:"

    -- printMatrix (inverseMatrix a)
    _ <- evaluate (inverseMatrix a)
    return ()