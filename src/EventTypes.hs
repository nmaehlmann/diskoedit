module EventTypes where
import Block
import qualified Level

type MouseStatusData = Bool

type TileSelectData = Block

type RectSelection = (Maybe Level.CellPosition, Maybe Level.CellPosition)
