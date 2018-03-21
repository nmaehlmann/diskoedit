{-# LANGUAGE RankNTypes #-}
module Level where

import qualified Data.Map as Map
import Data.Maybe
import Block

type Level = (Map.Map (Int, Int) Block)

type CellPositionData = (Int, Int)

type CellUpdate = ([CellPositionData], Block)

width :: Int
width = 74

height :: Int
height = 46

allCellPositions :: [(Int, Int)]
allCellPositions =
    [(x, y) | y <- [0 .. (height - 1)], x <- [0 .. (width - 1)]]

getBlock :: (Int, Int) -> Level -> Block
getBlock pos blockMap = fromJust $ Map.lookup pos blockMap

setBlock :: (Int, Int) -> Block -> Level -> Level
setBlock = Map.insert

empty :: Level
empty = foldr (flip Map.insert Air) Map.empty allCellPositions
