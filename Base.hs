import Graphics.Gloss
import Graphics.Gloss.Juicy
import Codec.Picture
import Codec.Picture.RGBA8
import Data.Complex

width = 512
height = width `div` 2

mandelbrotness :: Complex Float -> Complex Float -> Float
mandelbrotness c s = (fromIntegral $ length $ takeWhile (\c -> magnitude c <= 2) $ take maxIter $ z c) / (fromIntegral maxIter) where
	z c = iterate (\x -> x^2 + c) s
	maxIter = 32

image :: Float -> Image PixelRGB8
image time = generateImage (\x y -> let [x',y',w,h] = map fromIntegral [x,y,width,height] in mandelbrotColor ((4*x'/w - 2.5) :+ (2*y'/h - 1)) (0.1*time :+ 0) ) width height where
	mandelbrotColor c s = PixelRGB8 (floor $ 255 * mandelbrotness c s) 120 120

main = animate (InWindow "Mandelbrot" (width,height) (0,0)) black (fromImageRGB8 . image)
