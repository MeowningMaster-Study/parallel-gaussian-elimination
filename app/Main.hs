module Main where

import Gauss
import Matrix
import Text.Printf (printf)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Conc (numCapabilities)

main :: IO ()
main = do
  let size = 50
  putStrLn $ "Matrix size: " ++ show size ++ "x" ++ show size
  putStrLn $ "Parallel (" ++ show numCapabilities ++ " cores)"
  (mat1, vec1) <- generateRandom size
  start <- getCurrentTime
  let ans = gaussianElimination mat1 vec1 True
  print ans
  end <- getCurrentTime
  putStrLn $ "Time: " ++ show (end `diffUTCTime` start)
