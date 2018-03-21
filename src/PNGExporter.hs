module PNGExporter (saveLevelAsPNG) where

import Block
import Codec.Picture
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import qualified Level

saveLevelAsPNG :: String -> Level.Level -> IO ()
saveLevelAsPNG filename lvl = do
    mapping <- loadTileImages
    let png = toPNG mapping lvl
    savePngImage filename (ImageRGB8 png)


toPNG :: (Map.Map Block (Image PixelRGB8)) -> Level.Level -> (Image PixelRGB8)
toPNG mapping lvl = generateImage (f mapping lvl) (Level.width * 10) (Level.height * 10)

f :: (Pixel p) => (Map.Map Block (Image p)) -> Level.Level -> Int -> Int -> p
f mapping lvl x y = pixelAt (fromJust (Map.lookup blockType mapping)) srcX srcY
  where
    blockType = Level.getBlock (blockX, blockY) lvl
    blockX = div x 10
    blockY = div y 10
    srcX = mod x 10
    srcY = mod y 10

loadTileImages :: IO (Map.Map Block (Image PixelRGB8))
loadTileImages = foldM (\mapping b -> loadTileImage mapping b (toPath b)) Map.empty allBlocks

loadTileImage :: (Map.Map Block (Image PixelRGB8)) -> Block -> FilePath -> IO (Map.Map Block (Image PixelRGB8))
loadTileImage mapping blockType path = do
    (Right (ImageRGB8 img@(Image w h _))) <- readImage ("static/css/" ++ path)
    return $ Map.insert blockType img mapping