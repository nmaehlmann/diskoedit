module Buttons where
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Block
import qualified Level
import qualified PNGExporter
import TMXParser
import EventTypes
    
createSaveButton :: Behavior Level.Level -> String -> UI Element
createSaveButton currentLevel filepath = do
    btnSave <- UI.button # set text "save"
    on UI.click btnSave $ \_ ->
        liftIO $ do
            lvl <- currentValue currentLevel
            writeFile (filepath ++ "/map.tmx") $ toTMX lvl
            PNGExporter.saveLevelAsPNG (filepath ++ "/result.png") lvl
    return btnSave

createLoadButton :: Handler Level.LevelUpdate -> String -> UI Element
createLoadButton loadEventHandler filepath = do
    btnLoad <- UI.button # set text "load"
    on UI.click btnLoad $ \_ ->
        liftIO $ do
            lvlFile <- readFile (filepath ++ "/map.tmx")
            forM_ (toUpdates (dropSpaces lvlFile)) loadEventHandler
    return btnLoad

createTileButtons :: Handler TileSelectData -> [UI Element]
createTileButtons tileSelectHandler = map (mkTileButton tileSelectHandler) allBlocks

mkTileButton :: Handler TileSelectData -> Block -> UI Element
mkTileButton tileSelectHandler blockType = do
    btn <- UI.canvas # set UI.height (30) # set UI.width (30) #. toCss blockType
    on UI.click btn $ \_ -> liftIO $ tileSelectHandler blockType
    return btn

createToolButtons :: Handler EditingTool -> [UI Element]
createToolButtons toolSelectHandler = map (mkToolButton toolSelectHandler) allTools

mkToolButton :: Handler EditingTool -> EditingTool -> UI Element
mkToolButton toolSelectHandler tool = do
    btn <- UI.button # set text (show tool)
    on UI.click btn $ \_ -> liftIO $ toolSelectHandler tool
    return btn