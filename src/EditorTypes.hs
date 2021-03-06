module EditorTypes where

import Block
import qualified Level

type MouseStatusData = Bool

type TileSelectData = Block

data RectSelection
    = NoBoundSpecified
    | OneBoundSpecified Level.CellPosition
    | BothBoundsSpecified Level.CellPosition
                          Level.CellPosition

data EditingTool
    = Pen
    | Rect
    deriving (Show, Eq, Ord)
    
allTools :: [EditingTool]    
allTools = [Pen, Rect]