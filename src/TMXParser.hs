module TMXParser where

import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Data.List
import LevelData
import Block

allTiles = normString tMXprefix >> manyTill tileParser (try (normString tMXpostfix))

tileParser = between (normString "<tile gid=\"") (normString "\"/>\n") nat

tMXprefix = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> \n <data>"
tMXpostfix = "</data>"

toUpdates :: String -> [CellUpdate]
toUpdates lvlFile = map merge groupedTiles
    where (Right tileList) = parse allTiles "" (dropSpaces lvlFile)
          indexedTileList = zip (map fromBlockNumber tileList) allCellPositions 
          groupedTiles = groupBy (\x y -> fst x == fst y) indexedTileList

normString = string . dropSpaces

merge :: [(Block, CellPositionData)] -> CellUpdate
merge ((b,p):pairs) = (p : map snd pairs,b)

toTMX :: Level -> String
toTMX lvl = tMXprefix ++ tiles  ++ tMXpostfix
    where tiles = concatMap toTile allCellPositions
          toTile pos = "<tile gid=\"" ++ show (toBlockNumber (getBlock pos lvl)) ++ "\"/>\n"

dropSpaces :: String -> String
dropSpaces = filter (\c -> c /= ' ' && c /= '\r' && c/= '\n')