import Data.Complex
import Data.Maybe (listToMaybe, isNothing, maybe, catMaybes)
import Control.Monad
import Control.Applicative ((<$>))
import System.Environment (getArgs)
import System.Console.GetOpt
import Control.Arrow ((***), second)

type Point = Complex Double
type Image c = Point -> c
type Fractal c = Int -> Image c -- | given an iteration count, generates an image

fractal ::     (Point -> (a -> a))  -- | iterated function
            -> (a -> Maybe c)       -- | convergence criterion
            -> (Point -> a)         -- | iteration start            
            -> Int                  -- | iteration limit
            -> Image (Maybe (Int, c))
fractal f cc sf l p = listToMaybe . catMaybes .  zipWith ((<$>) . (,)) [0..l] . map cc . iterate (f p) . sf $ p

newton ::      (Point -> Point)     -- | the function
            -> (Point -> Point)     -- | its derivative
            -> Fractal (Maybe (Int, Point))
newton f f' = fractal (const next)
                    (\z -> if next z == z then Just z else Nothing)
                    id
    where
        next z = z - f z/f' z

mandelbrot :: Fractal (Maybe (Int, ()))
mandelbrot = fractal (\c z -> z*z + c)
                    (\z -> if magnitude z > 2 then Just () else Nothing)
                    (const 0)

julia :: Point -> Fractal (Maybe (Int, ()))
julia p = fractal (\c z -> z*z + p)
                  (\z -> if magnitude z > 2 then Just () else Nothing)
                  id

convergence :: Image (Maybe (Int, a)) -> Image Int
convergence = fmap (maybe 0 ((+1) . fst))

writePgm :: Image Int -> Point -> Point -> Double -> IO ()
writePgm f ll ur scale =
    do
        putStrLn "P5"
        print width 
        print height 
        putStrLn "255"
        forM_ [0.. height-1] $ \i -> let y = fromIntegral i/scale in
            forM_ [0.. width-1] $ \j -> let x = fromIntegral j/scale in
                let z = (x :+ y) + ll in (putChar . toEnum . f $ z)
    where
        width = floor $ scale * realPart (ur-ll)
        height = floor $ scale * imagPart (ur-ll)

fractSpec :: FractSpec -> Fractal Int
fractSpec Mandelbrot = convergence . mandelbrot
fractSpec (Julia p) = convergence . julia p
fractSpec (Newton roots) = convergence . newton f f'
    where 
        f z = product $ map (z-) roots
        f' z = let factors = map (z-) roots
                in sum . map product . dropEach $ factors
        dropEach [] = []
        dropEach (x:xs) = xs:map (x:) (dropEach xs)

data FractSpec = Newton [Point] | Mandelbrot | Julia Point
data Options = Options
                { optScale :: Double
                , optBotLeft :: Complex Double
                , optTopRight :: Complex Double
                , optType :: FractSpec
                }

defaultOptions = Options
                    { optScale=100.0
                    , optBotLeft=negate (1 :+ 1)
                    , optTopRight=(1 :+ 1)
                    , optType=Mandelbrot
                    }

options :: [OptDescr (Options -> Options)]
options = [ Option ['s'] ["scale"] (ReqArg (\x opt -> opt { optScale=read x } ) "SCALE")
                "scale 1 unit to SCALE pixels"
          , Option [] ["ll", "lower-left"] (ReqArg (\x opt -> opt { optBotLeft=readC x } ) "X,Y")
                "set lower left of viewport to X,Y"
          , Option [] ["ur", "upper-right"] (ReqArg (\x opt -> opt { optTopRight=readC x } ) "X,Y")
                "set upper right of viewport to X,Y"
          , Option ['m'] ["mandelbrot"] (NoArg (\opt -> opt { optType=Mandelbrot } ))
                "render the Mandelbrot set"
          , Option ['j'] ["julia"] (ReqArg (\x opt -> opt { optType=Julia (readC x) } ) "X,Y")
                "render the Julia set at X,Y"
          , Option ['n'] ["newton"] (ReqArg (\x opt -> opt { optType=Newton (readCs x) } ) "X1,Y1:X2,Y2:...")
                "render the Newton fractal for a polynomial with roots at X1,Y1, X2,Y2, ..."
        ]
    where
        readC :: String -> Complex Double
        readC = uncurry (:+) . (read *** (read . tail)) . span (/= ',')
        readCs = map readC . split

        split :: String -> [String]
        split [] = []
        split (':':xs) = split xs
        split xs = uncurry (:) . second split . span (/= ':') $ xs

getOpts :: [String] -> IO Options
getOpts argv =
   case getOpt Permute options argv of
      (o,[],[]  ) -> return $ foldl (flip ($)) defaultOptions o
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: fractal [OPTION...]"

main = do
        opts <- getOpts =<< getArgs
        writePgm (fractSpec (optType opts) 254)
                 (optBotLeft opts) (optTopRight opts)
                 (optScale opts)
