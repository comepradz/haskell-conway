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
  , gliderSW
  , moveCells
  , block
  , blinkerV
  , blinkerH
  , beaconFull
  , beaconEmpty
  , toCells
  , toadH
  , blockLayingSwitchEngine
  , blockLayingSwitchEngine2
  ) where

import qualified Data.Array.Unboxed as Arr
import qualified Control.Parallel.Strategies as P

type LifeGridData = Arr.UArray (Int, Int) Bool
type LifeCycleFunction = Bool -> Int -> Bool
type Cell = ((Int, Int), Bool)
data LifeGrid = LifeGrid Bool LifeCycleFunction LifeGridData

instance Show LifeGrid where
  show = reverse . tail . reverse . showGrid

pDM :: P.NFData b => (a -> b) -> [a] -> [b]
pDM = P.parMap P.rdeepseq

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
showGrid g@(LifeGrid _ _ d) = unlines allLines
  where
    height = snd $ gridSize g
    charFor b = if b then '*' else ' '
    items = Arr.assocs d
    allLines = pDM line $ Arr.range (0, height -1)
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
    newContent = pDM (\(i@(x,y),v) -> (i, f v $ liveNeighbourCount g x y)) $
                     Arr.assocs d


gliderSW :: [Cell]
gliderSW = [((1,0), True),
            ((2,1), True),
            ((0,2), True),
            ((1,2), True),
            ((2,2), True)]

moveCells :: (Int, Int) -> [Cell] -> [Cell]
moveCells (x, y) = map moveCell
  where
    moveCell ((x', y'), v) = (((x + x'), (y + y')), v)

block :: [Cell]
block = [((0, 0), True),
         ((0, 1), True),
         ((1, 0), True),
         ((1, 1), True)]

blinkerV :: [Cell]
blinkerV = [((0, 0), True),
            ((0, 1), True),
            ((0, 2), True)]

blinkerH :: [Cell]
blinkerH = [((0, 0), True),
            ((1, 0), True),
            ((2, 0), True)]

beaconFull :: [Cell]
beaconFull = block ++ (moveCells (2,2) block)

beaconEmpty :: [Cell]
beaconEmpty = filter okay beaconFull
  where
    okay (i,_)
      | i == (1,1) = False
      | i == (2,2) = False
      | otherwise = True

toCells :: String -> [Cell]
toCells s = concat . map cellLine $ numberedlines
  where
    numberedlines = zip [0..] $ lines s
    cellLine (l,s') = map (\(x,c) -> ((x,l),c == '*')) $ zip [0..] s'

toadH :: [Cell]
toadH = toCells " ***\n*** "

blockLayingSwitchEngine :: [Cell]
blockLayingSwitchEngine = toCells "           * *\n**        *\n**         *  *\n             ***"

blockLayingSwitchEngine2 :: [Cell]
blockLayingSwitchEngine2 = toCells "*** *\n*    \n   **\n ** *\n* * *"

