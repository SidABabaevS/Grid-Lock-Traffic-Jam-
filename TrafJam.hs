{-# LANGUAGE FlexibleContexts #-}
import System.Environment
import Graphics.UI.GLFW           
import Graphics.Rendering.OpenGL
import GHC.Float
import System.Exit
import Data.Functor
import Control.Monad
import Data.Maybe

title = "Traffic Jam"

winSize :: GLsizei

winSize = 600
   
data Car = Car  -- ^ Вертикально расположенная машина.
   {
     dir :: Integer, -- 1 == vert, 0 == horiz
     x :: Integer,  -- Местоположение машины (её «головы»).
     y :: Integer,
     lngth :: Integer,  -- Длина машины в клетках доски.
     carColor :: Color4 GLfloat
   }
  deriving (Eq, Ord, Show)

data Dir = ToLeft | ToRight | ToUp | ToDown
  deriving (Eq, Show)  
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
  toCar(map (read) (words x)::[Integer]):parseCars xs

toCar ([dir,x,y,ln]) = if dir == 0 && y == 2 then Car dir x y ln green else Car dir x y ln red

loop cars = do
  display cars
  newCarsMouse <- mouseOnCar cars
  newCarsKeyboard <- keyboardOnCar newCarsMouse  
  loop newCarsKeyboard


gX (Car _ x _ _ _) = x
gY (Car _ _ y _ _) = y

keyboardOnCar :: [Car] -> IO [Car]
keyboardOnCar cars = do
  l <- getKey 65
  r <- getKey 68
  u <- getKey 87
  d <- getKey 83
  if l == Press then do
    sleep 0.2
    return (turnLeft cars)
  else if r == Press then do
      sleep 0.2
      return (turnRight cars)
  else if u == Press then do
      sleep 0.2
      return (turnUp cars)
  else if d == Press then do
      sleep 0.2
      return (turnDown cars)
  else return cars

turnLeft :: [Car] -> [Car]
turnLeft cars = do
  let c = findOrangeCar cars
  if isJust c then
	if getDirection (fromJust c) == 0 then (moveCars cars cars (fromJust c) ToLeft) else cars 
  else cars	

turnRight :: [Car] -> [Car]
turnRight cars = do
  let c = findOrangeCar cars
  if isJust c then
	if getDirection (fromJust c) == 0 then (moveCars cars cars (fromJust c) ToRight) else cars 
  else cars
   
turnUp :: [Car] -> [Car]
turnUp cars = do
  let c = findOrangeCar cars
  if isJust c then
	if getDirection (fromJust c) == 1 then (moveCars cars cars (fromJust c) ToUp) else cars 
  else cars
   
turnDown :: [Car] -> [Car]
turnDown cars = do
  let c = findOrangeCar cars
  if isJust c then
	if getDirection (fromJust c) == 1 then (moveCars cars cars (fromJust c) ToDown) else cars 
  else cars	  

moveCars :: [Car] -> [Car] -> Car -> Dir -> [Car]
moveCars _ [] _ _ = []
moveCars cars (c:cs) car dir = 
  if c == car then 
    (moveCar cars car dir):(moveCars cars cs car dir) 
  else c:(moveCars cars cs car dir)

moveCar :: [Car] -> Car -> Dir -> Car
moveCar cars (carToMove@(Car d x y ln col)) direc 
  | direc == ToUp = if isEmpty cars (getAddCoordXY carToMove (0,(-1))) carToMove then (Car d x (y-1) ln col) else carToMove
  | direc == ToDown = if y+ln+1<=6 && isEmpty cars (getAddCoordXY carToMove (0,ln)) carToMove then (Car d x (y+1) ln col) else carToMove
  | direc == ToLeft = if isEmpty cars (getAddCoordXY carToMove ((-1),0)) carToMove then (Car d (x-1) y ln col) else carToMove
  | direc == ToRight = if x+ln+1<=6 && isEmpty cars (getAddCoordXY carToMove (ln,0)) carToMove then (Car d (x+1) y ln col) else carToMove

isEmpty ::  [Car] -> (Integer,Integer) -> Car -> Bool
isEmpty [] _ _ = True
isEmpty ((c@(Car d x y ln _)):cs) (xx,yy) carToMove = 
  if inField (xx,yy) then do
    --let [(x1,y1),p2,(x2,y2),p4] = coords2 c :: [(Integer,Integer)] --xx <= x2 && y1 <= yy && yy <= y2 
    if c /= carToMove && ((d == 1 && x == xx && y <= yy && yy < y+ln) || 
      (d == 0 && y == yy && x <= xx && xx < x+ln)) then False
--    if x1-3 <= xx && y1-3 <= yy && xx+100 <= x2+3 && yy+100 <= y2+3 then False       
    else isEmpty cs (xx,yy) carToMove
  else False
  
inField (x, y) = x >= 0 && x < 600 && y >= 0 && y < 600
   
findOrangeCar :: [Car] -> Maybe Car
findOrangeCar [] = Nothing
findOrangeCar (c:cs) = if (getColor c) == orange then 
    Just c
  else findOrangeCar cs
   
mouseOnCar :: [Car] -> IO [Car]
mouseOnCar cars = do
  mbl <- getMouseButton ButtonLeft
  --mbr <- getMouseButton ButtonRight
  if (mbl == Press) then do
    --putStrLn "LEFT"
    mpos <- get mousePos 
    updateCars cars mpos
  else do
    --putStrLn "RIGHT"    
    return cars
  --when (mbr == Press) (putStrLn "PRESSED Right!")

updateCars :: [Car] -> Position -> IO [Car]
updateCars cars p@(Position x y) = do
  --putStrLn (show x++"  " ++show y)
  let foundedCar = inCar p cars
  if isJust foundedCar then
    return (changeTheCar cars (fromJust foundedCar))
  else return cars
  
changeTheCar :: [Car] -> Car -> [Car]
changeTheCar [] c = []
changeTheCar (c1@(Car dir x y ln col):xs) c2@(Car dirC xC yC _ _) = 
  if dir == dirC && x == xC && y == yC then 
    (Car dir x y ln orange):(changeTheCar xs c2) else
  if dir == 0 && y == 2 then (Car dir x y ln green):(changeTheCar xs c2) 
  else (Car dir x y ln red):(changeTheCar xs c2) 

inCar :: Position -> [Car] -> Maybe Car  
inCar _ [] = Nothing
inCar p@(Position a b) (c:cs) = do
  let [x1,x2,y1,y2] = getXYCar c
  let x = fromIntegral $ toInteger a
  let y = fromIntegral $ toInteger b
  if x1 < x && x < x2 && y1 < y && y < y2 then
    Just c
  else inCar p cs
  where
    getXYCar (Car dir xx yy len _) 
      | dir == 1 = [toScale1 xx, toScale2 (xx+1), toScale1 yy, toScale2 (yy+len)]
      | dir == 0 = [toScale1 xx, toScale2 (xx+len), toScale1 yy, toScale2 (yy+1)]
      | otherwise = error "Craft the Coords!"
  

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
orange = Color4 (1::GLfloat) 0.5 0 1

-- primitives

fatline :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
fatline ax ay bx by = renderPrimitive Lines $ do
    vertex2f ax ay
    vertex2f bx by
    vertex2f (ax-1) ay
    vertex2f (bx-1) by
    vertex2f (ax+1) ay
    vertex2f (bx+1) by

getColor (Car _ _ _ _ col) = col
getDirection (Car dir _ _ _ _) = dir
getAddCoordXY (Car _ x y _ _) (a,b) = (x+a,y+b)
getCarLength (Car _ _ _ ln _) = ln

--х у len vert\goriz 
carCoord :: Car -> IO ()
carCoord c = 
  letsDraw (mapM_ (uncurry vertex2f) (coords c)) (getColor c)

--coords :: Fractional Integer => Car -> 
coords (c@(Car dir x y len _ ))
  | dir == 1 = [(toScale1 x, toScale1 y),(toScale2 (x+1), toScale1 y), 
							(toScale2 (x+1), toScale2 (y+len)), (toScale1 x, toScale2 (y+len))]
  | dir == 0 = [(toScale1 x, toScale1 y),(toScale2 (x+len), toScale1 y),
							(toScale2 (x+len), toScale2 (y+1)), (toScale1 x, toScale2 (y+1))]
  | otherwise = error "Create the Coords!"

coords2 (c@(Car dir x y len _ ))
  | dir == 1 = [(toScale11 x, toScale11 y),(toScale22 (x+1), toScale11 y), 
							(toScale22 (x+1), toScale22 (y+len)), (toScale11 x, toScale22 (y+len))]
  | dir == 0 = [(toScale11 x, toScale11 y),(toScale22 (x+len), toScale11 y),
							(toScale22 (x+len), toScale22 (y+1)), (toScale11 x, toScale22 (y+1))]
  | otherwise = error "Create the Coords!"

  
letsDraw coords col = do
  color col
  renderPrimitive Quads coords  

toScale1 n = realToFrac(n * 100)+3
toScale2 n = realToFrac(n * 100)-3

toScale11 n = (n * 100)+3
toScale22 n = (n * 100)-3

