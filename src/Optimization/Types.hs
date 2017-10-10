{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Strict          #-}

module Optimization.Types where

import Control.Lens
import Numeric.LinearAlgebra            ( Numeric )
import Numeric.LinearAlgebra.Data
import Data.Vector.Storable       as VS ( map
                                        , singleton
                                        , zipWith )

{- |Basic "zippy" Num instance for storable vectors. Note that, while this is defined
    for some types of Vector by hmatrix, it unfortunately does not account for any
    Num (Vector a). -}
instance Numeric a => Num (Vector a) where
  a + b    = VS.zipWith (+)   a b
  a - b    = VS.zipWith (-)   a b
  a * b    = VS.zipWith (*)   a b
  abs a    = VS.map  (abs)    a
  signum a = VS.map  (signum) a
  fromInteger i = VS.singleton (fromInteger i)

{- |Termination criterion for a numerical optimization method.
    We terminate either on maxit iterations or once our guesses are within
    threshold of one another. -}
data TerminationCriterion a = TerminateAt {
  _maxit     :: Maybe Int, -- ^Maximum number of iterations (if any).
  _threshold :: Maybe a    -- ^Threshold used to declare convergence (if any).
  } deriving (Show, Eq)

-- |Step parameters for Nelder-Mead simplex search. a must be Storable and Num.
data NMStepParameters a = NMStepParameters {
  _epsilon :: a, -- ^Offset around initial guess to construct starting simplex.
  _alpha   :: a, -- ^Reflection coefficient for Nelder-Mead algorithm.
  _gamma   :: a, -- ^Expansion coefficient for Nelder-Mead algorithm.
  _rho     :: a, -- ^Contraction coefficient for Nelder-Mead algorithm.
  _sigma   :: a  -- ^Shrink coefficient for Nelder-Mead algorithm.
  } deriving (Show, Eq)

-- |Standard Nelder-Mead parameters.
defaultStep :: NMStepParameters Double
defaultStep = NMStepParameters 1.0 1.0 2.0 0.5 0.5

-- |The Nelder-Mead simplex.
type Simplex a = Matrix a

-- |Create a TerminationCriterion with just a convergence threshold.
terminateAt ::
  a ->                   -- ^Convengence threshold
  TerminationCriterion a -- ^A TerminationCriterion for optimization.
terminateAt t = TerminateAt Nothing (Just t)

-- |Create a TerminationCriterion with just a maximum iteration threshold.
terminateAfter ::
  Int               ->   -- ^Maximum number of iterations.
  TerminationCriterion a -- ^A TerminationCriterion for optimization.
terminateAfter m = TerminateAt (Just m) Nothing

-- |Create a TerminationCriterion with a convergence threshold and maximum iteration
-- |limit.
terminateAtOrAfter ::
  a      ->              -- ^Convergence threshold
  Int    ->              -- ^Maximum number of iterations
  TerminationCriterion a -- ^A TerminationCriterion for optimization.
terminateAtOrAfter t m = TerminateAt (Just m) (Just t)

makeLenses ''TerminationCriterion
makeLenses ''NMStepParameters
