import System.Environment
import Graphics.UI.GLFW           
import Graphics.Rendering.OpenGL
import GHC.Float
import System.Exit
import Data.Functor

title = "Traffic Jam"

winSize :: GLsizei

winSize = 600
   
data Car = Car  -- ^ Вертикально расположенная машина.
   {
     dir :: Int, -- 1 == vert, 2 == horiz
     x :: Int,  -- ^ Местоположение машины (её «головы»).
     y :: Int,
     lngth :: Int  -- ^ Длина машины в клетках доски.
   }
  deriving (Eq, Ord, Show)

--cars :: [Car]

main = do
    initialize
    openWindow (Size winSize winSize) [] Window
    windowTitle $= title

    clearColor $= Color4 1 1 1 1
    ortho (0) (600) (600) (0) (-1) 1
   
    windowCloseCallback $= exitWith ExitSuccess
    windowSizeCallback  $= (\size -> viewport $= (Position 0 0, size))
    args <- getArgs
    if length args /= 1 then do
      putStrLn "FILE!"
      return()
    else do
      cars <- getCarsFromFile (args!!0)
      loop cars
    --let cars = [(Car 1 1 1 3),(Car 0 0 0 2),(Car 0 2 4 2)]

getCarsFromFile :: FilePath -> IO [Car]
getCarsFromFile f = do 
  content <- readFile f
  return (parseCars (lines content))

parseCars :: [String] -> [Car]
parseCars [] = []
parseCars (x:xs) = do
  toCar(map (read) (words x)::[Int]):parseCars xs

toCar ([dir,x,y,ln]) = Car dir x y ln

loop cars = do
    display cars
    loop cars

display cars = do
  clear [ColorBuffer] 
  color red 
  printCars cars
  --circle 0 0 10
  color green
  fatline 598 200 598 300
  --circle 600 600 10
  swapBuffers

printCars [] = return()
printCars (x:xs) = do
		carCoord x
		printCars xs

vertex2f :: GLfloat -> GLfloat -> IO ()
vertex2f a b = vertex (Vertex3 a b 0)

-- colors

white = Color4 (0::GLfloat)
black = Color4 (0::GLfloat) 0 0 1
red   = Color4 (1::GLfloat) 0 0 1
green = Color4 (0::GLfloat) 1 0 1

-- primitives

fatline :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
fatline ax ay bx by = renderPrimitive Lines $ do
    vertex2f ax ay
    vertex2f bx by
    vertex2f (ax-1) ay
    vertex2f (bx-1) by
    vertex2f (ax+1) ay
    vertex2f (bx+1) by

--х у len vert\goriz 
carCoord :: Car -> IO ()
carCoord c = 
  renderPrimitive Quads $ mapM_ (uncurry vertex2f) (coords c)
  where
    coords (c@(Car dir x y len))
      | dir == 1 = [(toScale1 x, toScale1 y),(toScale2 (x+1), toScale1 y), 
                                (toScale2 (x+1), toScale2 (y+len)), (toScale1 x, toScale2 (y+len))]
      | dir == 0 = [(toScale1 x, toScale1 y),(toScale2 (x+len), toScale1 y),
                                (toScale2 (x+len), toScale2 (y+1)), (toScale1 x, toScale2 (y+1))]
      | otherwise = error "Craft the Coords!"

toScale1 n = realToFrac(n * 100)+3
toScale2 n = realToFrac(n * 100)-3

{-circle :: GLfloat -> GLfloat -> GLfloat -> IO ()
circle cx cy rad = 
  renderPrimitive Polygon $ mapM_ (uncurry vertex2f) points
  where n = 50
        points = zip xs ys
        xs = fmap (\x -> cx + rad * sin (2*pi*x/n)) [0 .. n]
        ys = fmap (\x -> cy + rad * cos (2*pi*x/n)) [0 .. n] -}
