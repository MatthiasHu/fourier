
import Fourier

import Codec.Picture  -- using JuicyPixels
import Codec.Picture.Types
import System.Environment
import System.FilePath.Posix
import Control.Applicative


main :: IO ()
main = do
  [f] <- getArgs
  Right img <- fmap (
       pixelMap pixelToF
     . extractLumaPlane
     . convertRGB8 )
     <$> readImage f
  let w = imageWidth img
      h = imageHeight img
      generator x y = unscale $
        dct2d (w, h) (uncurry (pixelAt img)) (x, y)
      unscale = (* sqrt (4 / fromIntegral (w-1) / fromIntegral (h-1)))
      fOut = dropExtension f <.> "out" <.> "png"
  writePng fOut . pixelMap fToPixel $ generateImage generator w h

-- mapping 128 to 0
pixelToF :: Pixel8 -> Float
pixelToF a = fromIntegral a - 128

fToPixel :: Float -> Pixel8
fToPixel a = round (a+128)
