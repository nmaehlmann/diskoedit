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

allTools :: [EditingTool]    
allTools = [Pen, Rect]

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

    -- an event firing in case of a rectange edit carrying a list of cell positions
    rectEditEvent <-
        fmap (fmap rectangleToPositions) $
        accumE NoBoundSpecified $
        fmap accumRectangle $
        whenE mouseDown $
        whenE (selectedTool `isTool` Rect) mouseEnterEvent

    -- an event firing in case of a pen edit carrying a list of cell positions
    penEditEvent <-
        return $
        fmap singleton $
        whenE mouseDown $
        whenE (selectedTool `isTool` Pen) mouseEnterEvent        

    -- merges pen and rectangle edits and adds the currently selected tile
    userEditEvent <- return $ positionsToLevelUpdate <$> selectedTile <@> (mergeEvents [penEditEvent, rectEditEvent])

    -- event 
    levelUpdateEvent  <- return $ filterE (not . null) $ mergeEvents [userEditEvent, loadEvent]
    
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

singleton :: a -> [a]
singleton = pure

positionsToLevelUpdate :: Block -> [Level.CellPosition] -> Level.LevelUpdate
positionsToLevelUpdate b p = (p, b)

mkTileButtons :: Handler TileSelectData -> [UI Element]
mkTileButtons tileSelectHandler = map (mkTileButton tileSelectHandler) allBlocks

mkTileButton :: Handler TileSelectData -> Block -> UI Element
mkTileButton tileSelectHandler blockType = do
    btn <- UI.canvas # set UI.height (tileSize * 3) # set UI.width (tileSize * 3) #. toCss blockType
    on UI.click btn $ \_ -> liftIO $ tileSelectHandler blockType
    return btn

mkToolButtons :: Handler EditingTool -> [UI Element]
mkToolButtons toolSelectHandler = map (mkToolButton toolSelectHandler) allTools

mkToolButton :: Handler EditingTool -> EditingTool -> UI Element
mkToolButton toolSelectHandler tool = do
    btn <- UI.button # set text (show tool)
    on UI.click btn $ \_ -> liftIO $ toolSelectHandler tool
    return btn

mergeEvents :: [Event a] -> Event a
mergeEvents = foldr (unionWith pickFirst) never
    where pickFirst x _ = x

boundsToPositions :: Level.CellPosition -> Level.CellPosition -> [Level.CellPosition]
boundsToPositions (x1, y1) (x2, y2) =
    [(x, y) | x <- fromTo (min x1 x2) (max x1 x2), y <- fromTo (min y1 y2) (max y1 y2)]
  where
    fromTo a b = take (b - a + 1) [a ..]

accumRectangle :: Level.CellPosition -> RectSelection -> RectSelection
accumRectangle bound2 (OneBoundSpecified bound1) = BothBoundsSpecified bound1 bound2
accumRectangle bound _ = OneBoundSpecified bound

rectangleToPositions :: RectSelection -> [Level.CellPosition]
rectangleToPositions (BothBoundsSpecified b1 b2) = boundsToPositions b1 b2
rectangleToPositions _ = []


selectedTool `isTool` tool = fmap (== tool) selectedTool 
