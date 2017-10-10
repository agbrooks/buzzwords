module Main where

import qualified Numeric.LinearAlgebra.Data

import Optimization.Types
import Optimization.NelderMead

trivialOpt :: Vector Double -> Double
trivialOpt v
  | length v != 3 = error "Vector is not length-3!"
  | otherwise     = ((v ! 0)**2) * (((v ! 1) - 1)**2) * (((v ! 2) - 2) ** 2)

main :: IO ()
main = do
  let
    tc      =  terminateAtOrAfter 0.001 1000
    optimum =  nelderMead tc trivialOpt (fromList [10.0, 12.1, -4.4])

  putStrLn $ show optimum
    
  
  
