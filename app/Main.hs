{-# LANGUAGE TemplateHaskell, RankNTypes #-}
import qualified Control.Lens as Lens
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.IORef
import Data.Maybe
import qualified Data.Map as M
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Codec.Picture
import Codec.Picture.Types
import LevelData
import TMXParser



tileSize = 10

data AppState = AppState {_mouseDown :: Bool, _currentBlockType :: BlockType, _level :: Level}
Lens.makeLenses ''AppState

data EditingTool = Pen | Rect | Fill
    deriving (Show,Eq)
tools = [Pen,Rect]

type MouseStatusData = Bool
type TileSelectData = BlockType
type RectSelection = (Maybe CellPositionData, Maybe CellPositionData)

data EventCollection = EventCollection {
    _mouseStatusHandler :: Handler MouseStatusData,
    _mouseEnterHandler :: Handler CellPositionData,
    _tileSelectHandler :: Handler TileSelectData,
    _toolSelectHandler :: Handler EditingTool,
    _updateCellEvent :: Event ([CellPositionData], BlockType)  }
Lens.makeLenses ''EventCollection

main :: IO ()
main = do
    static <- return "static"
    startGUI defaultConfig {
        jsStatic = Just static,
        jsCallBufferMode = BufferRun } setup

setup :: Window -> UI ()
setup w = void $ do
    (mouseStatusEvent, mouseStatusEventHandler) <- liftIO $ newEvent
    (mouseEnterEvent, mouseEnterEventHandler) <- liftIO $ newEvent
    (tileSelectEvent, tileSelectEventHandler) <- liftIO $ newEvent
    (toolSelectEvent, toolSelectEventHandler) <- liftIO $ newEvent
    (loadEvent, loadEventHandler) <- liftIO $ newEvent

    selectedTileBehaviour <- stepper Solid tileSelectEvent
    selectedToolBehaviour <- stepper Pen toolSelectEvent
    mouseStatusBehavior <- stepper False mouseStatusEvent

    accumulatedRectSelection <- accumE (Nothing,Nothing) $ pure accumRectSelection <@> (whenE mouseStatusBehavior  (whenE (pure (==Rect) <*> selectedToolBehaviour) mouseEnterEvent))
    cellRectEvent <- return $ filterJust $ apply (pure calculateCellPositionsFromEvent) accumulatedRectSelection
    
    rectEditEvent <- return $ whenE mouseStatusBehavior  $ whenE (pure (==Rect) <*> selectedToolBehaviour)  $ pure toEditCellData2  <*> selectedTileBehaviour <@> cellRectEvent
    penEditEvent <- return $ whenE mouseStatusBehavior  $ whenE (pure (==Pen) <*> selectedToolBehaviour)  $ pure toEditCellData <*> selectedTileBehaviour <@> mouseEnterEvent
    
    editCellDataEvent <- return $ mergeEvents [rectEditEvent, penEditEvent, loadEvent]

    accumulatedLevelUpdate <- accumE emptyLevel $ (pure editLevel) <@> editCellDataEvent
    
    currentLevelBehaviour <- stepper emptyLevel accumulatedLevelUpdate
    
    onEvent tileSelectEvent $ \tile -> do liftIO $ putStrLn $ toName tile
    
    eventCollection <- return EventCollection {_mouseStatusHandler = mouseStatusEventHandler,
        _mouseEnterHandler = mouseEnterEventHandler,
        _tileSelectHandler = tileSelectEventHandler,
        _toolSelectHandler = toolSelectEventHandler,
        _updateCellEvent = editCellDataEvent}
    
    btnSave <- UI.button
        # set text "save"
    on UI.click btnSave $ \_ -> liftIO $ do
        lvl <- currentValue currentLevelBehaviour
        writeFile "generated/map.tmx" $ toTMX lvl
        mapping <- loadTileImages
        let png = toPNG mapping lvl
        savePngImage "generated/result.png" (ImageRGB8 png)
        return ()

    btnLoad <- UI.button
        # set text "load"
    on UI.click btnLoad $ \_ -> liftIO $ do
        lvlFile <- readFile "generated/map.tmx"
        forM_ (toUpdates (dropSpaces lvlFile)) loadEventHandler  

    return w # set title "Editor"
    UI.addStyleSheet w "editor.css"
    getBody w #+ [mkTable eventCollection] #+ mkTileButtons eventCollection #+ [return btnLoad, return btnSave] #+ mkToolButtons eventCollection
    flushCallBuffer
 

toEditCellData :: BlockType -> CellPositionData -> CellUpdate
toEditCellData b p = ([p],b)

toEditCellData2 :: BlockType -> [CellPositionData] -> CellUpdate
toEditCellData2 b p = (p,b)

editLevel ::  CellUpdate -> Level -> Level
editLevel (cellPositions, blockType) lvl = foldr (\singelPos -> M.insert singelPos blockType) lvl cellPositions

mkTable :: EventCollection-> UI Element
mkTable eventCollection =  UI.table #+ map (\y -> mkTableRow eventCollection y) [0..mapHeight-1]

mkTableRow :: EventCollection -> Int -> UI Element    
mkTableRow eventCollection y = UI.tr #+ map (\x -> mkCell eventCollection (x,y)) [0..mapWidth-1]

mkCell :: EventCollection -> (Int,Int) -> UI Element
mkCell eventCollection cellPos = do 
    cell <- UI.td
        # set UI.height tileSize
        # set UI.width  tileSize
        #. "tile"
        #. (toCss Air)
        
    on UI.mousedown cell $ \_ -> do 
        liftIO $ (Lens.view mouseStatusHandler eventCollection) True
        liftIO $ (Lens.view mouseEnterHandler eventCollection) cellPos
    on UI.mouseup cell $ \_ -> liftIO $ (Lens.view mouseStatusHandler eventCollection) False
    on UI.hover cell $ \_ -> liftIO $ (Lens.view mouseEnterHandler eventCollection) cellPos
    onEvent (Lens.view updateCellEvent eventCollection) $ \(eventPositions, blockType) -> do
        when (elem cellPos eventPositions) $ void $ (element cell) #. (toCss blockType)
            
    return cell
                
mkTileButtons :: EventCollection -> [UI Element]
mkTileButtons eventCollection = map (mkTileButton eventCollection) blocks

mkTileButton :: EventCollection -> BlockType -> UI Element
mkTileButton eventCollection blockType = do
    btn <- UI.canvas 
        # set UI.height (tileSize*3)
        # set UI.width  (tileSize*3)
        #.toCss blockType
    on UI.click btn $ \_ -> liftIO $ (Lens.view tileSelectHandler eventCollection) blockType
    return btn

mkToolButtons :: EventCollection -> [UI Element]
mkToolButtons eventCollection = map (mkToolButton eventCollection) tools

mkToolButton :: EventCollection -> EditingTool -> UI Element
mkToolButton eventCollection tool = do
    btn <- UI.button # set text (show tool)
    on UI.click btn $ \_ -> liftIO $ (Lens.view toolSelectHandler eventCollection) tool
    return btn

toPNG :: (M.Map BlockType (Image PixelRGB8)) -> Level -> (Image PixelRGB8)
toPNG mapping lvl = generateImage (f mapping lvl) (mapWidth * 10) (mapHeight * 10)

f :: (Pixel p) => (M.Map BlockType (Image p)) -> Level -> Int -> Int -> p
f mapping lvl x y = pixelAt (fromJust (M.lookup blockType mapping)) srcX srcY
    where blockType = getBlock (blockX,blockY) lvl
          blockX = div x 10
          blockY = div y 10 
          srcX = mod x 10
          srcY = mod y 10

loadTileImages :: IO (M.Map BlockType (Image PixelRGB8))
loadTileImages = foldM (\mapping b -> loadTileImage mapping b (toPath b)) M.empty blocks

loadTileImage :: (M.Map BlockType (Image PixelRGB8)) -> BlockType -> FilePath -> IO (M.Map BlockType (Image PixelRGB8))
loadTileImage mapping blockType path = do
    (Right (ImageRGB8 img@(Image w h _))) <- readImage ("static/css/" ++ path)
    return $ M.insert blockType img mapping

mergeEvents :: [Event a] -> Event a
mergeEvents = foldr (unionWith pickFirst) never

pickFirst :: a -> a -> a
pickFirst a _ = a

calculateCellPositions :: CellPositionData -> CellPositionData -> [CellPositionData]
calculateCellPositions (x1,y1) (x2,y2) = [(x,y) | x <- fromTo (min x1 x2) (max x1 x2), y <- fromTo (min y1 y2) (max y1 y2)]
    where fromTo a b = take (b - a + 1) [a..]

accumRectSelection :: CellPositionData -> RectSelection -> RectSelection
accumRectSelection newPos ((Just x), Nothing) = ((Just x), (Just newPos))
accumRectSelection newPos _ = ((Just newPos), Nothing)

calculateCellPositionsFromEvent :: RectSelection -> Maybe [CellPositionData]
calculateCellPositionsFromEvent (p1,p2) = calculateCellPositions <$> p1 <*> p2

