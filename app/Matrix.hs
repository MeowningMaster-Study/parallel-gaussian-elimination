module Matrix where

import Data.List (elemIndices)
import System.Random (randoms, getStdGen, newStdGen)

infinity :: Float
infinity = -1e10

epsilon :: Float
epsilon = 1e-8

type Row = [Float]
type Matrix = [Row]
type Vector = [Float]
type Point = (Int, Int)

subtractRows :: Row -> Row -> Row
subtractRows [] _ = []
subtractRows _ [] = []
subtractRows (x:xs) (y:ys) = (x - y) : subtractRows xs ys

multiplyRow :: Row -> Float -> Row
multiplyRow [] _ = []
multiplyRow (x:xs) value = x * value : multiplyRow xs value

valueToMul :: Int -> Row -> Row -> Float
valueToMul ind x y
    | ind < length x && ind < length y && ind >= 0 = x !! ind / y !! ind
    | otherwise = 0

updateVector :: Vector -> Float -> Int -> Vector
updateVector vec newValue ind
    | ind < length vec && ind >= 0 = take ind vec ++ [newValue] ++ drop (ind + 1) vec
    | otherwise = vec

maxValue :: Row -> Float
maxValue [] = infinity
maxValue [x] = infinity
maxValue (x:xs)
    | x > val = x
    | otherwise = val
    where
        val = maxValue xs

maxIndex :: Matrix -> [Int] -> Float -> Int -> Point -> Point
maxIndex [] _ _ _ ind = ind
maxIndex (x:xs) usedRows value rowId ind
    | elem rowId usedRows || value >= maxValue x = maxIndex xs usedRows value (rowId + 1) ind
    | value < maxValue x && maxValue x /= 0 = maxIndex xs usedRows (maxValue x) (rowId + 1) (rowId, newInd)
    | otherwise = (-1, -1)
    where
        newInd = head $ elemIndices (maxValue x) x

addVectorToMatrix :: Matrix -> Vector -> Matrix
addVectorToMatrix [] [] = []
addVectorToMatrix [] _ = []
addVectorToMatrix _ [] = []
addVectorToMatrix (mat:mats) (vec:vecs) = (mat ++ [vec]) : addVectorToMatrix mats vecs

generateMatrix :: Int -> Int -> IO Matrix
generateMatrix 0 m = return []
generateMatrix n m = do
    g <- newStdGen
    let row = take m (randoms g :: [Float])
    ans <- generateMatrix (n - 1) m
    return (row : ans)

generateRandom :: Int -> IO (Matrix, Vector)
generateRandom n = do
    mat <- generateMatrix n n
    g <- newStdGen
    let vec = take n (randoms g :: [Float])
    return (mat, vec)