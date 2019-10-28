module Main where

import Codec.Picture
import Codec.Picture.Types
import System.Environment
import Codec.FFmpeg
import Codec.FFmpeg.Juicy
import Control.Monad (forM_)

-- Main function: generates mandelbrot images and then stitches them together into a video.
main :: IO ()
main = do
    putStrLn "Enter a non-negative beginning frame number"
    beginning <- getLine
    putStrLn "Enter a non-negative end frame number"
    ending <- getLine
    let start = sanitizeInput (read beginning :: Int)
    let end = sanitizeInput (read ending :: Int)
    putStrLn "Enter a number for red color parameter"
    col1 <- getLine
    putStrLn "Enter a number for green color parameter"
    col2 <- getLine
    putStrLn "Enter a number for blue color parameter"
    col3 <- getLine
    let c12 = ((read col1 :: Int), (read col2 :: Int), (read col3 :: Int))
    let frames = generateNImages start end c12
    putStrLn "Beginning video render."
    juicyToFFmpeg frames ("mandelbrot"++(show start)++(show end)++".mp4")
    putStrLn "Done"
    where
        sanitizeInput x
            | x < 0 = error "Number cannot be negative"
            | otherwise = x

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
params = EncodingParams 800 600 10 Nothing Nothing "" Nothing

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
generateNImages :: Int -> Int -> (Int, Int, Int) -> [Image PixelRGB8]
generateNImages start end c123
                | start == end = []
                | start > end = (generateFractal start c123):(generateNImages (start-1) end c123)
                | otherwise   = (generateNImages start (end-1) c123)++[(generateFractal end c123)]


offset :: (Double, Double)
-- offset = (0.3750001200618655, -0.2166393884377127) <- This is for a new focus point for zoom
offset = (0.099, 0.893983)

-- Defines how fast we zoom into a fractal
zoomfactor :: Double
zoomfactor = 0.03

-- Defines the width of each frame
width :: Int
width = 800

-- Defines the height of each frame
height :: Int
height = 600

-- Generates n-th Mandelbrot fractal frame for the video
generateFractal :: Int -> (Int, Int, Int) -> Image PixelRGB8
generateFractal n c123 = (generateImage (mandelbrot n c123)) width height

-- Number of iterations to go through to check if the number set is a Mandelbrot set
iters :: Int -> Int
iters n = 25 + n*n

-- Colour palette to choose colors from when we overshoot from iterations in iters
palette :: Int -> (Int, Int, Int) -> [PixelRGB8]
palette n (red, green, blue) = foldr (\a -> \b -> (PixelRGB8 (fromIntegral $a*red) (fromIntegral $a*green) (fromIntegral $a*blue)):b) [] [0..(iters n)]

-- Generates pixel for each number set with getColor function if set is in Mandelbrot else use palette
mandelbrot :: Int -> (Int, Int, Int) -> Int -> Int -> PixelRGB8
mandelbrot n c123 x0 y0 = getColor 0 0 0 n c123
    where getColor :: Double -> Double -> Int -> Int -> (Int, Int, Int) -> PixelRGB8
          getColor x y i n (r, g, b) = if x*x + y*y < 2*2 && i < (iters n)
                           then getColor (x*x - y*y + (scaleX x0 n))
                                         (2*x*y + (scaleY y0 n))
                                         (i+1)
                                         n
                                         (r,g,b)
                           else (palette n c123)!!i

-- Horizontally scales the image while zooming in
scaleX :: Int -> Int -> Double
scaleX x n = (3.5/ (zoomfactor*(fromIntegral (n*n*n)))) * (fromIntegral x) / (fromIntegral width) - fst offset

-- Vertically scales the image while zooming in
scaleY :: Int -> Int -> Double
scaleY y n = (2/ (zoomfactor*(fromIntegral (n*n*n)))) * (fromIntegral y) / (fromIntegral height) - snd offset
