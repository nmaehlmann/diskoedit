module Block where

import qualified Data.Bimap as BiMap
import Data.Maybe

data Block
    = Air
    | Solid
    | Cloud
    | Spike
    | SuperSolid
    | PlayerSpawn
    | Item
    deriving (Eq, Ord, Read, Show, Enum)

toCss :: Block -> String
toCss Solid = "solidTile"
toCss Spike = "spikesTile"
toCss Cloud = "cloudTile"
toCss Air = "airTile"
toCss SuperSolid = "superSolidTile"
toCss PlayerSpawn = "playerSpawnTile"
toCss Item = "itemTile"

toPath :: Block -> String
toPath Solid = "solid.png"
toPath Spike = "spike.png"
toPath Cloud = "cloud.png"
toPath Air = "air.png"
toPath SuperSolid = "superSolid.png"
toPath _ = "air.png"

blockMapping :: BiMap.Bimap Block Int
blockMapping =
    BiMap.fromList
        [ (Solid, 20)
        , (Spike, 9)
        , (Cloud, 2)
        , (Air, 6)
        , (SuperSolid, 11)
        , (PlayerSpawn, 7)
        , (Item, 10)
        ]

fromBlockNumber :: Int -> Block
fromBlockNumber k = fromMaybe Air $ BiMap.lookupR k blockMapping

toBlockNumber :: Block -> Int
toBlockNumber k = fromMaybe 6 $ BiMap.lookup k blockMapping

allBlocks :: [Block]
allBlocks = [toEnum 0 ..]
