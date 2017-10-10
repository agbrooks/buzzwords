{-# LANGUAGE FlexibleContexts #-} {- Container (Vector a) => ..., etc. -}
{-# LANGUAGE RankNTypes       #-} {- For type traces w/ ST             -}
{-# LANGUAGE Strict           #-} {- Lazy evaluation is no good here   -}
{-# LANGUAGE TemplateHaskell  #-} {- Since we're using lenses aplenty  -}

{-
   TODO LIST:
   * Make sure basic functionality works, write some tests.
   * Use Numeric.LinearAlgebra.Devel to guarantee that ALL matrix operations
     are zero-copy or low allocation overhead.
   * Leverage chunk-based parallelism (lots of embarassing parallelism!)
-}

module Optimization.NelderMead
  ( nelderMeadAt
  , nelderMeadFull
  , nelderMead
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.ST

import Data.Maybe
import Data.Ord   ( comparing )
import Data.STRef ( STRef
                  , modifySTRef
                  , newSTRef
                  , readSTRef
                  , writeSTRef )
       
import Data.Vector.Storable          as VS  hiding ( take
                                                   , forM_
                                                   , (!) )
import Data.Vector.Algorithms.Intro  ( sortBy )
import Data.Vector.Mutable           as VM  hiding ( take )
import Data.Vector.Storable.Mutable  as VSM hiding ( take )

import Numeric.LinearAlgebra.Data    as LA
import Numeric.LinearAlgebra.HMatrix as HM
import Numeric.LinearAlgebra.Devel   as MutM

import Optimization.Types

-- One point of the simplex and its associated score.
data Vertex s = Vertex {
  which    :: Int,
  score    :: s
  }

-- Ranked vertices for NM search.
data VertexRank a b = VertexRank {
  bestOf         :: Vertex b,
  nextBestOf     :: Vertex b,
  worstOf        :: Vertex b,
  goodCentroidOf :: Vector a
  }

-- Create a VertexRank from the available data.
pickBestAndWorstVerts ::
  (Numeric a, Fractional a, Ord b) =>
  --(Container Vector a, Element a, Ord b, Transposable (Simplex a) (Simplex a)) =>
  (Vector a -> b)  -> -- A function from a point on the simplex to some orderable type.
  Simplex a        -> -- The simplex to consider.
  VertexRank a b      -- Vertex rank.
pickBestAndWorstVerts f s = runST $ do
  let
    nDims  = rows s
    points = tr s

  scores <- VM.new nDims
  forM_ [0..(nDims - 1)] (\i -> VM.unsafeWrite scores i (i, f $ points ! i))
  sortBy (comparing snd) scores
  let
    mvertex j = liftM (uncurry Vertex) (VM.unsafeRead scores j)
  h  <- mvertex 0
  sh <- mvertex 1
  l  <- mvertex (nDims - 1)
  
  return $ VertexRank h sh l (center $ allButIth (which l) s)

-- Increment a termination criterion by one iteration.
increment :: TerminationCriterion a -> TerminationCriterion a
increment p
  | isJust (view maxit p) = over maxit (fmap pred) p
  | otherwise             = p

-- Compute the center of the NM Simplex.
center ::
  (Numeric a, Fractional a) =>
  Simplex a ->
  Vector a
center s = runST $ do  
  centroid <- VSM.new (rows s)
  
  forM_ [0..((rows s) - 1)] (\i -> do
    forM_ [0..((cols s) - 1)] (\j -> do
      let
        u = fromIntegral (cols s)
      VSM.unsafeWrite centroid i ((s `atIndex` (i,j)) / u) ))

  freeze centroid

-- Create a simplex with all points but the i-th.
allButIth :: (Numeric a) => Int -> Simplex a -> Simplex a
allButIth l s
  | l == 0        = subMatrix (0,1) (nDims, nDims) s
  | l == (cols s) = subMatrix (0,0) (nDims, nDims) s
  | otherwise     = ls ||| rs where
      nDims = rows s
      ls = subMatrix (0, l - 1) (nDims, l - 1)     s
      rs = subMatrix (0, l + 1) (nDims, nDims - l) s

-- The centroid of all points except whichever is worst.
goodCentroid :: (Numeric a, Fractional a, Ord b) =>
  VertexRank a b ->
  Simplex a      ->
  Vector a
goodCentroid r splx = center $ allButIth ((which . worstOf) r) splx

-- Get the i-th point of a simplex.
nthPoint :: (Numeric a, Element a) => Int -> Simplex a -> Vector a
nthPoint i s = flatten $ subMatrix (0, i) (rows s, 1) s

-- Get an estimate of simplex size by computing the sum of all
-- elements in the simplex, translated so that its centroid
-- lies at zero.
simplexSize :: (Numeric a, Fractional a) =>
  Simplex a ->
  a
simplexSize s = fastSimplexL1 (moveToOrigin s) where
  fastSimplexL1 m = runST $ do
    acc <- newSTRef 0
    mut <- unsafeThawMatrix m
    forM_ [0..((rows m) - 1)] $ \i -> do
      forM_ [0..((cols m) - 1)] $ \j -> do
        elem <- unsafeReadMatrix mut i j
        modifySTRef acc (+ elem)
    readSTRef acc

moveToOrigin :: (Numeric a, Fractional a) =>
  Simplex a ->
  Simplex a
moveToOrigin s = runSTMatrix $ do
  sm  <- thawMatrix s
  ctr <- unsafeThawVector (center s)

  forM_ [0..((rows s) - 1)] $ \i -> do
    c_entry <- unsafeReadVector ctr i
    forM_ [0..((cols s) - 1)] $ \j -> do
      m_entry <- unsafeReadMatrix sm i j
      unsafeWriteMatrix sm i j (m_entry - c_entry)

  return sm

-- Check if current simplex state and termination criterion indicate that we should
-- be finished.
isDone tc simplex = noMoreIters || closeEnough || neitherValid where
  noMoreIters  = (fromMaybe 1 (view maxit tc)) == 0
  thresh       = view threshold tc
  closeEnough  = fromMaybe False (fmap ((simplexSize simplex) <) thresh)
  neitherValid = (isNothing (view maxit tc)) && (isNothing (view threshold tc))

-- Update the i-th point of simplex sx with vector p.
updateSimplex :: (Numeric a) => Int -> Vector a -> Simplex a -> Simplex a
updateSimplex i v sx = runSTMatrix $ do
  sxm <- thawMatrix sx
  vm  <- unsafeThawVector v
  forM_ [0..((cols sx) - 1)] $ \d -> do
    updated <- unsafeReadVector vm d
    unsafeWriteMatrix sxm d i updated
  return sxm

-- Reflect the simplex's "worst" point around the hyperplane defined by the 
-- other points. If this constitutes a significant improvement, then consider
-- "expanding" even farther in the same direction. Otherwise, give up.
reflectAndExpand :: forall a b s. (Num a, Numeric a, Fractional a, Ord b) =>
  NMStepParameters a              ->
  (Vector a -> b)                 ->
  STRef s (Simplex a)             ->
  STRef s (VertexRank a b)        ->
  ST    s Bool
reflectAndExpand params f s vrank = do
  r  <- readSTRef vrank
  sx <- readSTRef s
      
  let
    x0      = goodCentroid r sx
    x_best  = nthPoint ((which . bestOf) r) sx
    sx_best = (score . bestOf)     r -- Score of best x
    sx_next = (score . nextBestOf) r -- Score of next best x
        
    a   = view alpha params
    xr  = x0 + ((x0 - x_best) `scaleBy` a) -- x reflected
    sxr = f xr                             -- score of x reflected

  -- How does the reflected point fare?
  if (sxr < sx_next && sxr > sx_best)
    then do
    modifySTRef s (updateSimplex ((which . worstOf) r) xr)
    return True
    else do

    -- Consider expansion.
    if (sxr < sx_best)
      then do
      let
        g   = view gamma params
        xe  = x0 + ((xr - x0) `scaleBy` g)
        sxe = f xe

      -- Is expansion better?
      if (sxe < sxr)
        then do
        modifySTRef s (updateSimplex ((which . worstOf) r) xe)
        return True
        else do
        modifySTRef s (updateSimplex ((which . worstOf) r) xr)
        return True
      else do
      return False

-- Move the worst point of the simplex inward toward the centroid of all the
-- better points. If this doesn't result in an improvement, don't do anything.
contract :: forall a b s. (Numeric a, Fractional a, Ord b) =>
  NMStepParameters a       ->
  (Vector a -> b)          ->
  STRef s (Simplex a)      ->
  STRef s (VertexRank a b) ->
  ST s Bool
contract params f s vrank = do
  r  <- readSTRef vrank
  sx <- readSTRef s
  let
    x0       = goodCentroid r sx
    x_worst  = nthPoint ((which . worstOf) r) sx
    sx_worst = (score . worstOf) r
        
    rh  = view rho params
    xc  = x0 + ((x_worst - x0) `scaleBy` rh)
    sxc = f xc

  if (sxc < sx_worst)
    then do
    modifySTRef s (updateSimplex ((which . worstOf) r) xc)
    return True
    else do
    return False      


-- Move all points of a simplex slightly towards the best point.
shrink :: forall a b s. (Num a, Fractional a, Storable a, Ord b) =>
  NMStepParameters a       ->
  (Vector a -> b)          ->
  STRef s (Simplex a)      ->
  STRef s (VertexRank a b) ->
  ST s Bool
shrink params f s vrank = do
  sx <- readSTRef s
  r  <- readSTRef vrank
  let
    sig    = view sigma r
    x_best = nthPoint ((which . bestOf) r) sx

  forM_ (\i -> do
            modifySTRef s (\simplex ->
                             if (i == ((which . bestOf) r))
                             then
                               simplex
                             else
                               let
                                 xi  = nthPoint i simplex
                                 xi' = x_best + (sig * (xi - x_best)) in
                                 updateSimplex i xi' simplex
                          )
        ) [0..((cols sx) - 1)]
  return True

  
-- |Predict the location of a minimum of an objective function using the Nelder-Mead simplex
--  search. This function permits tuning of all parameters, including the starting simplex.
nelderMeadFull :: (Num a, Fractional a, Storable a, Ord b) =>
  NMStepParameters a     -> -- ^Nelder-Mead step parameters
  TerminationCriterion a -> -- ^Reason we would terminate
  (Vector a -> b)        -> -- ^Objective function
  Simplex a              -> -- ^Starting simplex
  Vector a                  -- ^Predicted minimum
nelderMeadFull params term_conditions func initial = (center . runST) $ do
  
  simplex  <- newSTRef initial
  vtx_rank <- newSTRef (pickBestAndWorstVerts simplex)

  nelderMeadStep params term_conditions func simplex vtx_rank where
          
    nelderMeadStep p tc f s vrank = do
      -- Update rank and see if we're done.
      modifySTRef vrank (pickBestAndWorstVerts func)
      finished <- isDone <$> readSTRef s

      if (finished)
        then do
        rnk <- readSTRef vrank
        sx  <- readSTRef s
        return $ nthPoint ((which . bestOf) rnk) sx

        -- Attempt to improve simplex.
        else do
        reflected <- reflectAndExpand p f s vrank
        unless reflected $ do
          contracted <- contract p f s vrank
          unless contracted $ do
            shrink p f s vrank
            
        nelderMeadStep p (iterate tc) f s vrank
        
-- |Predict the location of a minimum of an objective function using the
--  Nelder-Mead simplex search. This version is like nelderMeadFull, but accepts
--  a starting point rather than an entire simplex.
nelderMeadAt :: (Num a, Fractional a, Storable a, Ord b) =>
  NMStepParameters a     -> -- ^Nelder-Mead step parameters
  TerminationCriterion a -> -- ^Reason we would terminate
  (Vector a -> b)        -> -- ^Objective function
  Vector a               -> -- ^Starting point.
  Vector a                  -- ^Predicted minimum.
nelderMeadAt p t f i = nelderMeadFull p t f (makeSimplexAround i e) where
  e = view epsilon p

-- |Minimize an objective function using the Nelder-Mead simplex search. This
--  invokation uses "default" step parameters.
nelderMead :: (Num a, Fractional a, Storable a, Ord b) =>
  TerminationCriterion a -> -- ^Reason we would terminate
  (Vector a -> b)        -> -- ^Objective function
  Vector a                -- ^Initial guess
nelderMead tc f x = nelderMeadAt defaultStep tc f x

{-
   Construct a simplex around a point by creating another vertex epsilon away from the initial
   guess along each vector of the canonical basis.
-}
makeSimplexAround :: (Element a) =>
  Vector a -> -- One vertex of the simplex
  a        -> -- Epsilon used to expand the simplex along the canonical basis
  Simplex a   -- Resultant simplex.
makeSimplexAround initial = fromBlocks [[asColumn initial, cloned + identity]]
  where
    dimension = size initial
    cloned    = asColumns (take dimension $ repeat initial)
    identity  = ident dimension

scaleBy :: (Num a) => Vector a -> a -> Vector a
scaleBy v a = runST $ do
  vm <- thawVector
  forM_ [0..((VS.length v) - 1)] $ \i -> do
    e <- unsafeReadVector vm i
    unsafeWriteVector vm i (e * a)
  freezeVector vm
