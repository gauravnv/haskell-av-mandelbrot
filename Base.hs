import Codec.Picture
import Codec.Picture.Types
import System.Environment

main :: IO ()
main = do
    putStrLn "Beginning rendering of image."
    makenframes 1 100

makenframes :: Int -> Int -> IO ()
makenframes n end
 							| n >= end = putStrLn "Completed all"
							| otherwise    = do
																savePngImage ("frames\\"++(show n)++".jpg") (generateFractal n)
																putStrLn ("Completed"++(show n))
																makenframes (n+1) end
offset :: (Float, Float)
offset = (0.1, 0.9)

zoomfactor :: Float
zoomfactor = 0.75

width :: Int
width = 200

height :: Int
height = 100

generateFractal :: Int -> DynamicImage
generateFractal n = ImageRGB8 $ (generateImage (mandelbrot n)) width height

iters :: Int
iters = 25

palette :: [PixelRGB8]
palette = foldr (\a -> \b -> (PixelRGB8 (fromIntegral a*15) (fromIntegral a*10) 5):b) [] [0..iters]

mandelbrot :: Int -> Int -> Int -> PixelRGB8
mandelbrot n x0 y0 = getColor 0 0 0 n
    where getColor :: Float -> Float -> Int -> Int -> PixelRGB8
          getColor x y i n = if x*x + y*y < 2*2 && i < iters
                           then getColor (x*x - y*y + (scaleX x0 n)) (2*x*y + (scaleY y0 n)) (i+1) n
                           else palette!!i

scaleX :: Int -> Int -> Float
scaleX x n = (3.5/ (zoomfactor*(fromIntegral n)+1) * (fromIntegral x)) / (fromIntegral width) - fst offset-- - zoomfactor * (fromIntegral n))

scaleY :: Int -> Int -> Float
scaleY y n = (2/ (zoomfactor*(fromIntegral n)+1) * (fromIntegral y)) / (fromIntegral height) - snd offset-- - zoomfactor * (fromIntegral n))
