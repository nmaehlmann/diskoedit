module BlockTable
    ( createTable
    ) where

import Block
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified Level

cellSize = 10

type MouseStatusData = Bool

type UpdateCells = ([Level.CellPositionData], Block)

type CellPreparer = Element -> (Int, Int) -> UI Element

createTable :: Handler MouseStatusData -> Handler Level.CellPositionData -> Event UpdateCells -> UI Element
createTable mouseStatusHandler mousePositionHandler updateEvent =
    mkTable $ createCellPreparer mouseStatusHandler mousePositionHandler updateEvent

mkTable :: CellPreparer -> UI Element
mkTable cellPreparer = UI.table #+ map (\y -> mkTableRow cellPreparer y) [0 .. Level.height - 1]

mkTableRow :: CellPreparer -> Int -> UI Element
mkTableRow eventCollection y = UI.tr #+ map (\x -> mkCell eventCollection (x, y)) [0 .. Level.width - 1]

mkCell :: CellPreparer -> (Int, Int) -> UI Element
mkCell cellPreparer cellPos = do
    cell <- UI.td # set UI.height cellSize # set UI.width cellSize #. "tile" #. (toCss Air)
    cellPreparer cell cellPos

createCellPreparer :: Handler MouseStatusData -> Handler Level.CellPositionData -> Event UpdateCells -> CellPreparer
createCellPreparer mouseStatusHandler mousePositionHandler updateEvent =
    \cell cellPos -> do
        on UI.mousedown cell $ \_ -> do
            liftIO $ mouseStatusHandler True
            liftIO $ mousePositionHandler cellPos
        on UI.mouseup cell $ \_ -> liftIO $ mouseStatusHandler False
        on UI.hover cell $ \_ -> liftIO $ mousePositionHandler cellPos
        onEvent updateEvent $ \(eventPositions, blockType) ->
            when (elem cellPos eventPositions) $ void $ (element cell) #. (toCss blockType)
        return cell
