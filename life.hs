module Main (main) where

import Control.Monad (forM_)

import Flarn.Conway

csi :: String
csi = [toEnum 27, '[']

cls :: String
cls = csi ++ "?25l" ++ csi ++ "2J"

topleft :: String
topleft = csi ++ "1;1H"

main :: IO ()
main = do
  let empty = newGrid 60 20 True b3s23
  let glider = mergeCells empty [((1,0), True),
                                 ((2,1), True),
                                 ((0,2), True),
                                 ((1,2), True),
                                 ((2,2), True)]
  let grids = lifeCycle glider
  putStr cls
  forM_ (map (topleft ++) (map show grids)) putStr
