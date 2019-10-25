module Base where

import Codec.Picture
import Codec.Picture.Types
import System.Environment
import Codec.FFmpeg
import Codec.FFmpeg.Juicy
import Control.Monad (forM_)

-- Main function: generates mandelbrot images and then stitches them together into a video.
main :: IO ()
main = do
    putStrLn "Enter the beginning frame number"
    start <- getLine
    putStrLn "Enter the end frame number"
    end <- getLine
    putStrLn "Beginning rendering of image."
    let frames = generateNImages (read start :: Int) (read end :: Int)
    juicyToFFmpeg frames ("mandelbrot.avi")
    putStrLn "Done"

{-
    Parameters to specify details for the video output.
    EncodingParams comes from FFmpeg. The arguments of EncodingParams are:
        * width
        * height
        * frames per second
        * codec (defaults to codec inferred from file name, if Nothing)
        * pixel format (defaults to format based on the codec, if Nothing)
        * preset
        * Not sure? The documentation doesn't include a seventh argument, but
          the compiler complains if we don't provide it.
-}
params :: EncodingParams
params = EncodingParams 200 200 3 (Just avCodecIdMpeg4) Nothing "" Nothing

{- Given a list of images and a file path, stitch the images together with
   FFmpeg and save the resulting video at the given file path. -}
juicyToFFmpeg :: [Image PixelRGB8] -> FilePath -> IO ()
juicyToFFmpeg frames fp = do
                        -- Initialize FFmpeg
                        initFFmpeg
                        -- Create writer to write a video stream to the given file
                        writer <- imageWriter params fp
                        -- writer is a function: (Maybe (Image p) -> IO ())
                        {- For each of the images: wrap it in a Just and pass the 
                           result into the writer to append it to the video stream -}
                        forM_ frames (writer . Just)
                        {- "Nothing" must be included to close the output stream 
                           and properly terminate video encoding -}
                        writer Nothing

{- 
   Generate mandelbrot images, given a start frame and an end frame.
   The start frame and end frame are so that we the program can be run at the same time
   on multiple computers and manually stitch together those videos, since it takes a
   long time for the program to run.  
-}                      
generateNImages :: Int -> Int -> [Image PixelRGB8]
generateNImages start end
                | start > end = []
                | otherwise     = (generateFractal end):(generateNImages start (end-1))

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
