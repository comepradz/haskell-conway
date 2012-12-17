module Main (main) where

import System.Environment (getEnv)
import Control.Monad (liftM, forM_)

import Flarn.Conway

csi :: String
csi = [toEnum 27, '[']

cls :: String
cls = csi ++ "?25l" ++ csi ++ "2J"

topleft :: String
topleft = csi ++ "1;1H"

ttyLines :: IO Int
ttyLines = liftM read $ getEnv "LINES"

columns :: IO Int
columns = liftM read $ getEnv "COLUMNS"

main :: IO ()
main = do
  width <- columns
  height <- ttyLines
  let empty = newGrid width height True b3s23
  let filled = mergeCells empty $ moveCells (20, 10) blockLayingSwitchEngine2
  let grids = lifeCycle filled
  putStr cls
--  putStr (show filled)
  forM_ (map (topleft ++) (map (reverse . tail . reverse . show) grids)) putStr
