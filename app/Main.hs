{-# LANGUAGE RankNTypes #-}

import Control.Monad
import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Block
import qualified Level
import qualified PNGExporter
import TMXParser

tileSize = 10

data EditingTool
    = Pen
    | Rect
    deriving (Show, Eq)

tools = [Pen, Rect]

type MouseStatusData = Bool

type TileSelectData = Block

type RectSelection = (Maybe Level.CellPositionData, Maybe Level.CellPositionData)

type UpdateCells = ([Level.CellPositionData], Block)

data EventCollection = EventCollection
    { mouseStatusHandler :: Handler MouseStatusData
    , mouseEnterHandler :: Handler Level.CellPositionData
    , tileSelectHandler :: Handler TileSelectData
    , toolSelectHandler :: Handler EditingTool
    , updateCellEvent :: Event ([Level.CellPositionData], Block)
    }


main :: IO ()
main = do
    static <- return "static"
    startGUI defaultConfig {jsStatic = Just static, jsCallBufferMode = BufferRun} setup

setup :: Window -> UI ()
setup w =
    void $ do
        (mouseStatusEvent, mouseStatusEventHandler) <- liftIO $ newEvent
        (mouseEnterEvent, mouseEnterEventHandler) <- liftIO $ newEvent
        (tileSelectEvent, tileSelectEventHandler) <- liftIO $ newEvent
        (toolSelectEvent, toolSelectEventHandler) <- liftIO $ newEvent
        (loadEvent, loadEventHandler) <- liftIO $ newEvent
        selectedTileBehaviour <- stepper Solid tileSelectEvent
        selectedToolBehaviour <- stepper Pen toolSelectEvent
        mouseStatusBehavior <- stepper False mouseStatusEvent
        accumulatedRectSelection <-
            accumE (Nothing, Nothing) $
            pure accumRectSelection <@>
            (whenE mouseStatusBehavior (whenE (pure (== Rect) <*> selectedToolBehaviour) mouseEnterEvent))
        cellRectEvent <- return $ filterJust $ apply (pure calculateCellPositionsFromEvent) accumulatedRectSelection
        rectEditEvent <-
            return $
            whenE mouseStatusBehavior $
            whenE (pure (== Rect) <*> selectedToolBehaviour) $
            pure toEditCellData2 <*> selectedTileBehaviour <@> cellRectEvent
        penEditEvent <-
            return $
            whenE mouseStatusBehavior $
            whenE (pure (== Pen) <*> selectedToolBehaviour) $
            pure toEditCellData <*> selectedTileBehaviour <@> mouseEnterEvent
        editCellDataEvent <- return $ mergeEvents [rectEditEvent, penEditEvent, loadEvent]
        accumulatedLevelUpdate <- accumE Level.empty $ (pure editLevel) <@> editCellDataEvent
        currentLevelBehaviour <- stepper Level.empty accumulatedLevelUpdate
        onEvent tileSelectEvent $ \tile -> do liftIO $ putStrLn $ toCss tile
        eventCollection <-
            return
                EventCollection
                    { mouseStatusHandler = mouseStatusEventHandler
                    , mouseEnterHandler = mouseEnterEventHandler
                    , tileSelectHandler = tileSelectEventHandler
                    , toolSelectHandler = toolSelectEventHandler
                    , updateCellEvent = editCellDataEvent
                    }
        btnSave <- UI.button # set text "save"
        on UI.click btnSave $ \_ ->
            liftIO $ do
                lvl <- currentValue currentLevelBehaviour
                writeFile "generated/map.tmx" $ toTMX lvl
                PNGExporter.saveLevelAsPNG "generated/result.png" lvl
        btnLoad <- UI.button # set text "load"
        on UI.click btnLoad $ \_ ->
            liftIO $ do
                lvlFile <- readFile "generated/map.tmx"
                forM_ (toUpdates (dropSpaces lvlFile)) loadEventHandler
        return w # set title "Editor"
        UI.addStyleSheet w "editor.css"
        cellPreparer <- return (createCellPreparer mouseStatusEventHandler mouseEnterEventHandler editCellDataEvent)
        getBody w #+ [mkTable cellPreparer] #+ mkTileButtons eventCollection #+ [return btnLoad, return btnSave] #+
            mkToolButtons eventCollection
        flushCallBuffer

toEditCellData :: Block -> Level.CellPositionData -> Level.CellUpdate
toEditCellData b p = ([p], b)

toEditCellData2 :: Block -> [Level.CellPositionData] -> Level.CellUpdate
toEditCellData2 b p = (p, b)

editLevel :: Level.CellUpdate -> Level.Level -> Level.Level
editLevel (cellPositions, blockType) lvl = foldr (\singelPos -> Map.insert singelPos blockType) lvl cellPositions

mkTable :: CellPreparer -> UI Element
mkTable eventCollection = UI.table #+ map (\y -> mkTableRow eventCollection y) [0 .. Level.height - 1]

mkTableRow :: CellPreparer -> Int -> UI Element
mkTableRow eventCollection y = UI.tr #+ map (\x -> mkCell eventCollection (x, y)) [0 .. Level.width - 1]

mkCell :: CellPreparer -> (Int, Int) -> UI Element
mkCell cellPreparer cellPos = do
    cell <- UI.td # set UI.height tileSize # set UI.width tileSize #. "tile" #. (toCss Air)
    cellPreparer cell cellPos


type CellPreparer = Element -> (Int, Int) -> UI Element

createCellPreparer :: Handler MouseStatusData -> Handler Level.CellPositionData -> Event UpdateCells -> CellPreparer
createCellPreparer mouseStatusHandler mousePositionHandler updateEvent = \cell cellPos -> do
    on UI.mousedown cell $ \_ -> do
        liftIO $ mouseStatusHandler True
        liftIO $ mousePositionHandler cellPos
    on UI.mouseup cell $ \_ -> liftIO $ mouseStatusHandler False
    on UI.hover cell $ \_ -> liftIO $ mousePositionHandler cellPos
    onEvent updateEvent $ \(eventPositions, blockType) -> do
        when (elem cellPos eventPositions) $ void $ (element cell) #. (toCss blockType)
    return cell

mkTileButtons :: EventCollection -> [UI Element]
mkTileButtons eventCollection = map (mkTileButton eventCollection) allBlocks

mkTileButton :: EventCollection -> Block -> UI Element
mkTileButton eventCollection blockType = do
    btn <- UI.canvas # set UI.height (tileSize * 3) # set UI.width (tileSize * 3) #. toCss blockType
    on UI.click btn $ \_ -> liftIO $ (tileSelectHandler eventCollection) blockType
    return btn

mkToolButtons :: EventCollection -> [UI Element]
mkToolButtons eventCollection = map (mkToolButton eventCollection) tools

mkToolButton :: EventCollection -> EditingTool -> UI Element
mkToolButton eventCollection tool = do
    btn <- UI.button # set text (show tool)
    on UI.click btn $ \_ -> liftIO $ (toolSelectHandler eventCollection) tool
    return btn

mergeEvents :: [Event a] -> Event a
mergeEvents = foldr (unionWith pickFirst) never

pickFirst :: a -> a -> a
pickFirst a _ = a

calculateCellPositions :: Level.CellPositionData -> Level.CellPositionData -> [Level.CellPositionData]
calculateCellPositions (x1, y1) (x2, y2) =
    [(x, y) | x <- fromTo (min x1 x2) (max x1 x2), y <- fromTo (min y1 y2) (max y1 y2)]
  where
    fromTo a b = take (b - a + 1) [a ..]

accumRectSelection :: Level.CellPositionData -> RectSelection -> RectSelection
accumRectSelection newPos ((Just x), Nothing) = ((Just x), (Just newPos))
accumRectSelection newPos _ = ((Just newPos), Nothing)

calculateCellPositionsFromEvent :: RectSelection -> Maybe [Level.CellPositionData]
calculateCellPositionsFromEvent (p1, p2) = calculateCellPositions <$> p1 <*> p2


