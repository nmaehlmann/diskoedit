import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Block
import qualified Level
import BlockTable
import EditorTypes
import Buttons
import System.Environment (getArgs)
import System.IO
import qualified Graphics.UI.Threepenny.Widgets as Widgets

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    startGUI defaultConfig {jsStatic = Just "static", jsCallBufferMode = BufferRun, jsPort = getPort args} setup
    where getPort [] = Nothing
          getPort (port:_) = Just (read port)

setup :: Window -> UI ()
setup w = do
    
    -- set title
    return w # set title "Editor"
    -- set css styling
    UI.addStyleSheet w "editor.css"

    -- create events
    (mouseStatusEvent, mouseStatusEventHandler) <- liftIO $ newEvent
    (mouseEnterEvent, mouseEnterEventHandler) <- liftIO $ newEvent
    (loadEvent, loadEventHandler) <- liftIO $ newEvent
    
    (tileListBox, selectedTile) <- createListBox allBlocks
    (toolListBox, selectedTool) <- createListBox allTools

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

    -- an event merging pen and rectangle edits and adds the currently selected tile
    userEditEvent <- return $ positionsToLevelUpdate <$> selectedTile <@> (mergeEvents [penEditEvent, rectEditEvent])

    -- an event carrying prepocessed data to update the level and frontend
    levelUpdateEvent  <- return $ filterE (not . null) $ mergeEvents [userEditEvent, loadEvent]
    
    -- behaviour representing the level which is currently edited
    currentLevel <- accumB Level.empty $ (pure Level.editLevel) <@> levelUpdateEvent

    editorTable <- createTable mouseStatusEventHandler mouseEnterEventHandler levelUpdateEvent
        #. "editor"

    sideBar <- UI.div
        #. "sidebar"
        #+ (singleton (UI.p #+ [label "tiles:"]))    
        #+ (singleton (UI.p #+ [return tileListBox]))    
        #+ (singleton (UI.p #+ [label "tools:"]))    
        #+ (singleton (UI.p #+ [return toolListBox]))    
        #+ (singleton (UI.p #+ [createLoadButton loadEventHandler "generated"]))
        #+ (singleton (UI.p #+ [createSaveButton currentLevel "generated"]))
    
    editorContent <- UI.div #. "editorContent"
        #+ map return [editorTable, sideBar]
    
    getBody w 
        #+ singleton (return editorContent)

    flushCallBuffer

singleton :: a -> [a]
singleton = pure

positionsToLevelUpdate :: Block -> [Level.CellPosition] -> Level.LevelUpdate
positionsToLevelUpdate b p = (p, b)

mergeEvents :: [Event a] -> Event a
mergeEvents = foldr (unionWith pickFirst) never
    where pickFirst x _ = x

boundsToPositions :: Level.CellPosition -> Level.CellPosition -> [Level.CellPosition]
boundsToPositions (x1, y1) (x2, y2) =
    [(x, y) | x <- range (min x1 x2) (max x1 x2), y <- range (min y1 y2) (max y1 y2)]
  where range a b = take (b - a + 1) [a ..]

accumRectangle :: Level.CellPosition -> RectSelection -> RectSelection
accumRectangle bound2 (OneBoundSpecified bound1) = BothBoundsSpecified bound1 bound2
accumRectangle bound _ = OneBoundSpecified bound

rectangleToPositions :: RectSelection -> [Level.CellPosition]
rectangleToPositions (BothBoundsSpecified b1 b2) = boundsToPositions b1 b2
rectangleToPositions _ = []

isTool :: Functor f => f EditingTool -> EditingTool -> f Bool
selectedTool `isTool` tool = fmap (== tool) selectedTool


display :: Show a => a -> UI Element
display tool = do
    lbl <- UI.label # set text (show tool)
    return lbl

createListBox :: (Show a, Ord a) => [a] -> UI (Element, Behavior a)
createListBox elements@(first:_) = do
    listBox <- Widgets.listBox (pure elements) (pure (Just first)) (pure display)
    selectedElement <- stepper first $ filterJust $ Widgets.rumors $ Widgets.userSelection listBox
    listBoxElement <- UI.element listBox 
        # set UI.size (show (length elements))
        # set UI.width 40
    return (listBoxElement, selectedElement)

label :: String -> UI Element
label labelText = UI.label # set UI.text labelText