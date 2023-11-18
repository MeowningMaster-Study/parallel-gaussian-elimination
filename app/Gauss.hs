module Gauss where

import Matrix
import GHC.Conc (numCapabilities, pseq)
import System.Environment (getArgs)
import GHC.Conc.Sync (par)

data Solution = Solution Vector | NoSolution | MinusInfinity
    deriving (Show, Eq)

gaussianElimination :: Matrix -> Vector -> Bool -> Solution
gaussianElimination [] vec _ = NoSolution
gaussianElimination mat vec parallel = checkResult $ findPivot (addVectorToMatrix mat vec) [] parallel

processRows :: Matrix -> Int -> Row -> Point -> Bool -> Matrix
processRows [] _ _ _ _ = []
processRows (mat:mats) rowInd rowToSub maxInd@(maxRowInd, maxColInd) parallel
    | parallel && maxRowInd == rowInd = nextRowsResult `par` (force divideOnOwn `pseq` (divideOnOwn : nextRowsResult))
    | parallel = nextRowsResult `par` (force subResult `pseq` (subResult : nextRowsResult))
    | maxRowInd == rowInd = divideOnOwn : nextRowsResult
    | otherwise = subResult : nextRowsResult
    where
        subResult = if checkLessZero res
            then multiplyRow res (-1)
            else res
        divideOnOwn = multiplyRow mat (1 / (mat !! maxColInd))
        nextRowsResult = processRows mats (rowInd + 1) rowToSub maxInd parallel
        res = subtractRows mat (multiplyRow rowToSub (valueToMul maxColInd mat rowToSub))
        checkLessZero :: Row -> Bool
        checkLessZero [] = True
        checkLessZero [_] = True
        checkLessZero (xRow:xsRow)
            | xRow > 0 = False
            | otherwise = checkLessZero xsRow

force :: [a] -> ()
force [] = ()
force (x:xs) = x `pseq` force xs

findPivot :: Matrix -> [Int] -> Bool -> Matrix
findPivot mat usedRows parallel
    | row == -1 || col == -1 = mat
    | length usedRows /= length mat = findPivot (processRows mat 0 rowToSub maxInd parallel) (row : usedRows) parallel
    | otherwise = mat
    where
        maxInd@(row, col) = maxIndex mat usedRows infinity 0 (-1, -1)
        rowToSub = mat !! row

checkRow :: Row -> Int
checkRow (x:xs)
    | null xs && abs x < epsilon = -1
    | null xs = 0
    | abs x < epsilon = checkRow xs
    | otherwise = 1

checkResult :: Matrix -> Solution
checkResult [] = Solution []
checkResult (mat:mats)
    | ans == NoSolution = NoSolution
    | ans == MinusInfinity = MinusInfinity
    | curCheck == 0 = NoSolution
    | curCheck == -1 = MinusInfinity
    | otherwise = Solution value
    where
        curCheck = checkRow mat
        ans = checkResult mats
        value = case ans of
            Solution a -> last mat : a
            _ -> []

