module PNGIO (saveLevelAsPNG) where

import Codec.Picture
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import qualified Level
import Block

blocksize :: Int
blocksize = 10

saveLevelAsPNG :: String -> Level.Level -> IO ()
saveLevelAsPNG filename lvl = do
    mapping <- loadTileImages
    let png = toPNG mapping lvl
    savePngImage filename (ImageRGB8 png)


toPNG :: (Map.Map Block (Image PixelRGB8)) -> Level.Level -> (Image PixelRGB8)
toPNG mapping lvl = generateImage (determinePixel mapping lvl) (Level.width * blocksize) (Level.height * blocksize)

determinePixel :: (Pixel p) => (Map.Map Block (Image p)) -> Level.Level -> Int -> Int -> p
determinePixel mapping lvl x y = pixelAt (fromJust (Map.lookup blockType mapping)) srcX srcY
  where
    blockType = Level.getBlock (blockX, blockY) lvl
    blockX = div x blocksize
    blockY = div y blocksize
    srcX = mod x blocksize
    srcY = mod y blocksize

loadTileImages :: IO (Map.Map Block (Image PixelRGB8))
loadTileImages = foldM (\mapping b -> loadTileImage mapping b (toPath b)) Map.empty allBlocks

loadTileImage :: (Map.Map Block (Image PixelRGB8)) -> Block -> FilePath -> IO (Map.Map Block (Image PixelRGB8))
loadTileImage mapping blockType path = do
    (Right (ImageRGB8 img)) <- readImage ("static/css/" ++ path)
    return $ Map.insert blockType img mapping