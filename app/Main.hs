{-# LANGUAGE RankNTypes #-}

import Control.Monad
import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Block
import qualified Level
import qualified PNGExporter
import TMXParser
import BlockTable
import EventTypes

tileSize = 10

data EditingTool
    = Pen
    | Rect
    deriving (Show, Eq)

tools = [Pen, Rect]



main :: IO ()
main = do
    static <- return "static"
    startGUI defaultConfig {jsStatic = Just static, jsCallBufferMode = BufferRun} setup

setup :: Window -> UI ()
setup w = do
    
    -- set title
    return w # set title "Editor"
    -- set css styling
    UI.addStyleSheet w "editor.css"

    -- create events
    (mouseStatusEvent, mouseStatusEventHandler) <- liftIO $ newEvent
    (mouseEnterEvent, mouseEnterEventHandler) <- liftIO $ newEvent
    (tileSelectEvent, tileSelectEventHandler) <- liftIO $ newEvent
    (toolSelectEvent, toolSelectEventHandler) <- liftIO $ newEvent
    (loadEvent, loadEventHandler) <- liftIO $ newEvent

    -- behavior representing the selected tile
    selectedTile <- stepper Solid tileSelectEvent

    -- behavior representing the selected tool
    selectedTool <- stepper Pen toolSelectEvent

    -- behavior representing the mouse status
    mouseDown <- stepper False mouseStatusEvent

    accumulatedRectSelection <-
        accumE (Nothing, Nothing) $
        pure accumRectSelection <@>
        (whenE mouseDown (whenE (pure (== Rect) <*> selectedTool) mouseEnterEvent))
    cellRectEvent <- return $ filterJust $ apply (pure calculateCellPositionsFromEvent) accumulatedRectSelection
    rectEditEvent <-
        return $
        whenE mouseDown $
        whenE (pure (== Rect) <*> selectedTool) $
        pure toEditCellData2 <*> selectedTile <@> cellRectEvent
    penEditEvent <-
        return $
        whenE mouseDown $
        whenE (pure (== Pen) <*> selectedTool) $
        pure toEditCellData <*> selectedTile <@> mouseEnterEvent
    
    -- event 
    levelUpdateEvent  <- return $ mergeEvents [rectEditEvent, penEditEvent, loadEvent]
    
    -- behaviour representing the level which is currently edited
    currentLevel <- accumB Level.empty $ (pure Level.editLevel) <@> levelUpdateEvent

    btnSave <- UI.button # set text "save"
    on UI.click btnSave $ \_ ->
        liftIO $ do
            lvl <- currentValue currentLevel
            writeFile "generated/map.tmx" $ toTMX lvl
            PNGExporter.saveLevelAsPNG "generated/result.png" lvl
    btnLoad <- UI.button # set text "load"
    on UI.click btnLoad $ \_ ->
        liftIO $ do
            lvlFile <- readFile "generated/map.tmx"
            forM_ (toUpdates (dropSpaces lvlFile)) loadEventHandler

    blockTable <- createTable mouseStatusEventHandler mouseEnterEventHandler levelUpdateEvent
    getBody w #+ [return blockTable] #+ mkTileButtons tileSelectEventHandler #+ [return btnLoad, return btnSave] #+
        mkToolButtons toolSelectEventHandler
    flushCallBuffer

toEditCellData :: Block -> Level.CellPosition -> Level.LevelUpdate
toEditCellData b p = ([p], b)

toEditCellData2 :: Block -> [Level.CellPosition] -> Level.LevelUpdate
toEditCellData2 b p = (p, b)

mkTileButtons :: Handler TileSelectData -> [UI Element]
mkTileButtons tileSelectHandler = map (mkTileButton tileSelectHandler) allBlocks

mkTileButton :: Handler TileSelectData -> Block -> UI Element
mkTileButton tileSelectHandler blockType = do
    btn <- UI.canvas # set UI.height (tileSize * 3) # set UI.width (tileSize * 3) #. toCss blockType
    on UI.click btn $ \_ -> liftIO $ tileSelectHandler blockType
    return btn

mkToolButtons :: Handler EditingTool -> [UI Element]
mkToolButtons toolSelectHandler = map (mkToolButton toolSelectHandler) tools

mkToolButton :: Handler EditingTool -> EditingTool -> UI Element
mkToolButton toolSelectHandler tool = do
    btn <- UI.button # set text (show tool)
    on UI.click btn $ \_ -> liftIO $ toolSelectHandler tool
    return btn

mergeEvents :: [Event a] -> Event a
mergeEvents = foldr (unionWith pickFirst) never

pickFirst :: a -> a -> a
pickFirst a _ = a

calculateCellPositions :: Level.CellPosition -> Level.CellPosition -> [Level.CellPosition]
calculateCellPositions (x1, y1) (x2, y2) =
    [(x, y) | x <- fromTo (min x1 x2) (max x1 x2), y <- fromTo (min y1 y2) (max y1 y2)]
  where
    fromTo a b = take (b - a + 1) [a ..]

accumRectSelection :: Level.CellPosition -> RectSelection -> RectSelection
accumRectSelection newPos ((Just x), Nothing) = ((Just x), (Just newPos))
accumRectSelection newPos _ = ((Just newPos), Nothing)

calculateCellPositionsFromEvent :: RectSelection -> Maybe [Level.CellPosition]
calculateCellPositionsFromEvent (p1, p2) = calculateCellPositions <$> p1 <*> p2


