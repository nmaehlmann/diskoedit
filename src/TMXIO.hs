module TMXIO (loadLevelUpdatesFromTMX, saveLevelToTMX) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number
import Data.List
import qualified Level
import Block

loadLevelUpdatesFromTMX :: String -> IO [Level.LevelUpdate]
loadLevelUpdatesFromTMX filepath = do 
    lvlFile <- readFile filepath
    return $ toUpdates (dropSpaces lvlFile)

saveLevelToTMX :: FilePath -> Level.Level -> IO ()
saveLevelToTMX filepath lvl = do
    writeFile filepath $ toTMX lvl  

tmxParser :: Parser [Int]
tmxParser = normString tmxPrefix >> manyTill tileParser (try (normString tmxPostfix))

tileParser :: Parser Int
tileParser = between (normString "<tile gid=\"") (normString "\"/>\n") nat

tmxPrefix :: String
tmxPrefix = "<?xml version=\"1.0\" encoding=\"UTF-8\"?> \n <data>"

tmxPostfix :: String
tmxPostfix = "</data>"

toUpdates :: String -> [Level.LevelUpdate]
toUpdates lvlFile = map mergeUpdates groupedTiles
    where (Right tileList) = parse tmxParser "" (dropSpaces lvlFile)
          indexedTileList = zip (map fromBlockNumber tileList) Level.allCellPositions 
          groupedTiles = groupBy (\x y -> fst x == fst y) indexedTileList

normString :: String -> Parser String
normString = string . dropSpaces

mergeUpdates :: [(Block, Level.CellPosition)] -> Level.LevelUpdate
mergeUpdates ((b,p):pairs) = (p : map snd pairs,b)

toTMX :: Level.Level -> String
toTMX lvl = tmxPrefix ++ tiles  ++ tmxPostfix
    where tiles = concatMap toTile Level.allCellPositions
          toTile pos = "<tile gid=\"" ++ show (toBlockNumber (Level.getBlock pos lvl)) ++ "\"/>\n"

dropSpaces :: String -> String
dropSpaces = filter (\c -> c /= ' ' && c /= '\r' && c/= '\n')