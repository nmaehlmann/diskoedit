module Buttons (createLoadButton, createSaveButton) where
import Control.Monad
import qualified Graphics.UI.Threepenny as UI
import System.Directory
import Graphics.UI.Threepenny.Core
import qualified Level
import qualified PNGIO
import qualified TMXIO

    
createSaveButton :: Behavior Level.Level -> FilePath -> UI Element
createSaveButton currentLevel filepath = do
    btnSave <- UI.button # set text "save"
    on UI.click btnSave $ \_ ->
        liftIO $ do
            lvl <- currentValue currentLevel
            createDirectoryIfMissing True filepath
            TMXIO.saveLevelToTMX (filepath ++ "/map.tmx")  lvl
            PNGIO.saveLevelAsPNG (filepath ++ "/result.png") lvl
    return btnSave

createLoadButton :: Handler Level.LevelUpdate -> FilePath -> UI Element
createLoadButton loadEventHandler filepath = do
    btnLoad <- UI.button # set text "load"
    on UI.click btnLoad $ \_ ->
        liftIO $ do
            -- loading a level is just generating and firing update events from a file 
            updates <- TMXIO.loadLevelUpdatesFromTMX (filepath ++ "/map.tmx")
            forM_ updates loadEventHandler
    return btnLoad
