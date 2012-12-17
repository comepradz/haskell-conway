-----------------------------------------------------------------------------
-- |
-- Module      :  Flarn.Conway
-- Copyright   :  Daniel Silverstone <dsilvers@digital-scurf.org>
-- License     :  BSD-style (see the file COPYING)
--
-- Maintainer  :  dsilvers@digital-scurf.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functionality regarding the Game of Life
--
-----------------------------------------------------------------------------

module Flarn.Conway (
    newGrid
  , b3s23  
  , gridSize
  , mergeCells
  , setAlive
  , birthCell
  , killCell
  , lifeCycle
  ) where

import qualified Data.Array.Unboxed as Arr

type LifeGridData = Arr.UArray (Int, Int) Bool
type LifeCycleFunction = Bool -> Int -> Bool
data LifeGrid = LifeGrid Bool LifeCycleFunction LifeGridData

instance Show LifeGrid where
  show = showGrid

b3s23 :: Bool -> Int -> Bool
b3s23 s n
  | (s == False) && (n == 3) = True
  | (s == True) && (n < 2) = False
  | (s == True) && (n > 3) = False
  | otherwise = s

newGrid :: Int -> Int -> Bool -> LifeCycleFunction -> LifeGrid
newGrid x y w f = LifeGrid w f emptyGrid
  where
    x' = x - 1
    y' = y - 1
    xRange = (0, x')
    yRange = (0, y')
    gridRange = [(a,b) | a <- Arr.range xRange, b <- Arr.range yRange]
    emptyGrid = Arr.array ((0, 0), (x', y')) [(i, False) | i <- gridRange]

gridSize :: LifeGrid -> (Int, Int)
gridSize (LifeGrid _ _ d) = (xbound + 1, ybound + 1)
  where
    xbound = fst . snd $ Arr.bounds d
    ybound = snd . snd $ Arr.bounds d

showGrid :: LifeGrid -> String
showGrid g@(LifeGrid _ _ d) = unlines [line i | i <- Arr.range (0, height -1)]
  where
    height = snd $ gridSize g
    charFor b = if b then '*' else ' '
    items = Arr.assocs d
    line n = map (\p -> charFor $ snd p) $ filter (\p -> ((snd . fst $ p) == n)) items

mergeCells :: LifeGrid -> [((Int, Int), Bool)] -> LifeGrid
mergeCells (LifeGrid w f d) cs = LifeGrid w f (d Arr.// cs)

setAlive :: Bool -> LifeGrid -> Int -> Int -> LifeGrid
setAlive v g x y = mergeCells g [((x, y), v)]

birthCell :: LifeGrid -> Int -> Int -> LifeGrid
birthCell = setAlive True

killCell :: LifeGrid -> Int -> Int -> LifeGrid
killCell = setAlive False

cellNeighbours :: LifeGrid -> Int -> Int -> [(Int, Int)]
cellNeighbours g@(LifeGrid w _ _) x y = filter include allNeighbours
  where
    width = fst $ gridSize g
    height = snd $ gridSize g
    wrapXv xv
      | (w == True) && (xv == -1) = width - 1
      | (w == True) && (xv == width) = 0
      | otherwise = xv
    wrapYv yv
      | (w == True) && (yv == -1) = height - 1
      | (w == True) && (yv == height) = 0
      | otherwise = yv
    include (x',y')
      | (x' == -1) || (x' == width) || (y' == -1) || (y' == height) = False
      | otherwise = True
    allNeighbours = filter (/= (x,y)) [(wrapXv x', wrapYv y') |
                     x' <- Arr.range (x - 1, x + 1),
                     y' <- Arr.range (y - 1, y + 1)]
                         
liveNeighbourCount :: LifeGrid -> Int -> Int -> Int
liveNeighbourCount g@(LifeGrid _ _ d) x y = 
  length $ filter liveCell $ cellNeighbours g x y
  where
    liveCell i = d Arr.! i

lifeCycle :: LifeGrid -> [LifeGrid]
lifeCycle g@(LifeGrid w f d) = nextGrid : (lifeCycle nextGrid)
  where
    nextGrid = LifeGrid w f newData
    newData = Arr.array (Arr.bounds d) newContent
    newContent = [(i, f v $ liveNeighbourCount g x y) |
                  (i@(x,y),v) <- Arr.assocs d]

