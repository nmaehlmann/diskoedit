{-# LANGUAGE RankNTypes #-}
module LevelData where

import qualified Control.Lens as Lens
import qualified Data.Map as M
import Data.Maybe
import Data.Bimap as BM



mapWidth = 74
mapHeight = 46


type Level = (M.Map (Int, Int) BlockType)
type CellPositionData = (Int, Int)
type CellUpdate = ([CellPositionData], BlockType)

data BlockType = Air | Solid | Cloud | Spike | SuperSolid | PlayerSpawn | Item
    deriving (Eq, Ord, Read, Show)



toCss :: BlockType -> String
toCss Solid = "solidTile"
toCss Spike = "spikesTile"
toCss Cloud = "cloudTile"
toCss Air   = "airTile"
toCss SuperSolid = "superSolidTile"
toCss PlayerSpawn = "playerSpawnTile"
toCss Item = "itemTile"


toPath :: BlockType -> String
toPath Solid = "solid.png"
toPath Spike = "spike.png"
toPath Cloud = "cloud.png"
toPath Air   = "air.png"
toPath SuperSolid = "superSolid.png"
toPath _ = "air.png"

toId :: BlockType -> Int
toId k = fromMaybe 6 $ BM.lookup k blockMapping

toBlockType :: Int -> BlockType
toBlockType k = fromMaybe Air $ BM.lookupR k blockMapping 

blockMapping :: BM.Bimap BlockType Int
blockMapping = BM.fromList [(Solid,20),(Spike,9),(Cloud,2),(Air,6),(SuperSolid,11),(PlayerSpawn,7),(Item,10)]

toName :: BlockType -> String
toName = toCss

getBlock :: (Int, Int) -> Level -> BlockType
getBlock pos blockMap = fromJust $ M.lookup pos blockMap

setBlock :: (Int, Int) -> BlockType -> Level -> Level
setBlock = M.insert

blocks :: [BlockType]
blocks = [Solid, Spike, Cloud, Air, SuperSolid, PlayerSpawn, Item]

emptyLevel :: Level
emptyLevel = foldr 
    (flip M.insert Air) M.empty allCellPositions

allCellPositions = [(x,y) | y <- [0..(mapHeight-1)], x <- [0..(mapWidth-1)]]


    