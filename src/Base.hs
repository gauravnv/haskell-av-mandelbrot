module Base where

import Codec.Picture
import Codec.Picture.Types
import System.Environment
import Codec.FFmpeg
import Codec.FFmpeg.Juicy
import Control.Monad (forM_)

main :: IO ()
main = do
    putStrLn "Beginning rendering of image."
    -- makenframes listOfImages 1 10
    juicyToFFmpeg (generateNImages  177 180) ("mandelbrot.avi")

params :: EncodingParams
params = EncodingParams 200 200 3 (Just avCodecIdMpeg4) Nothing "" Nothing

juicyToFFmpeg :: [Image PixelRGB8] -> FilePath -> IO ()
juicyToFFmpeg frames fp = do
                        initFFmpeg
                        writer <- imageWriter params fp
                        -- give Just image data to writer to append it
                        forM_ frames (writer . Just)
                        writer Nothing -- finalize, or else you'll break it

generateNImages :: Int -> Int -> [Image PixelRGB8]

generateNImages start end
                |start > end = []
                |otherwise     = (generateFractal end):(generateNImages start (end-1))

-- makenframes :: Int -> Int -> IO ()
-- makenframes n end
--             | n >= end = putStrLn "Completed all, writing video"
--             | otherwise    = do
--                 savePngImage ("frames\\"++(show n)++".jpg") (generateFractal n)
--                 putStrLn ("Completed"++(show n))
--                 makenframes (n+1) end


offset :: (Double, Double)
offset = (0.099, 0.89398)

zoomfactor :: Double
zoomfactor = 0.03

width :: Int
width = 400

height :: Int
height = 300

generateFractal :: Int -> Image PixelRGB8
generateFractal n = (generateImage (mandelbrot n)) width height

iters :: Int -> Int
iters n = 25 + n*n

palette :: Int -> [PixelRGB8]
palette n = foldr (\a -> \b -> (PixelRGB8 (fromIntegral a*10) (fromIntegral a*5) (fromIntegral a*2)):b) [] [0..(iters n)]

mandelbrot :: Int -> Int -> Int -> PixelRGB8
mandelbrot n x0 y0 = getColor 0 0 0 n
    where getColor :: Double -> Double -> Int -> Int -> PixelRGB8
          getColor x y i n = if x*x + y*y < 2*2 && i < (iters n)
                           then getColor (x*x - y*y + (scaleX x0 n)) (2*x*y + (scaleY y0 n)) (i+1) n
                           else (palette n)!!i

scaleX :: Int -> Int -> Double
scaleX x n = (3.5/ (zoomfactor*(fromIntegral (n*n*n)))) * (fromIntegral x) / (fromIntegral width) - fst offset-- - zoomfactor * (fromIntegral n))

scaleY :: Int -> Int -> Double
scaleY y n = (2/ (zoomfactor*(fromIntegral (n*n*n)))) * (fromIntegral y) / (fromIntegral height) - snd offset-- - zoomfactor * (fromIntegral n))
