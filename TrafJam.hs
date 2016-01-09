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
   
data Car = Car
   {
     dir :: Integer, -- 1 - vert, 0 - horiz
     x :: Integer,  -- Местоположение машины (её «головы»).
     y :: Integer,
     lngth :: Integer,  -- Длина машины в клетках доски.
     carColor :: Color4 GLfloat
   }
  deriving (Eq, Ord, Show)

data Dir = ToLeft | ToRight | ToUp | ToDown
  deriving (Eq, Show)  

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

--Считывание машин из файла
getCarsFromFile :: FilePath -> IO [Car]
getCarsFromFile f = do 
  content <- readFile f
  return (parseCars (lines content))

--Разбитие строки, создание машин 
parseCars :: [String] -> [Car]
parseCars [] = []
parseCars (x:xs) = do
  toCar(map (read) (words x)::[Integer]):parseCars xs

--Если это главная машина, то у нее зеленый цвет, иначе - красный
toCar ([dir,x,y,ln]) = if dir == 0 && y == 2 then Car dir x y ln green else Car dir x y ln red

--Главный цикл
loop cars = do
  display cars
  let c = isOver cars
  if isJust c then do
    putStrLn "YOU WIN!"
    blinking (fromJust c)
  else do
    newCarsMouse <- mouseOnCar cars
    newCarsKeyboard <- keyboardOnCar newCarsMouse  
    loop newCarsKeyboard

--Условие финиша
isOver [] = Nothing
isOver (c@(Car d x y ln _):cs) = if d == 0 && y == 2 && x + ln - 1 == 5 then Just c 
  else isOver cs

--"Моргание"
blinking car = do
  swapBuffers
  letsDraw (mapM_ (uncurry vertex2f) (coords car)) cyan
  swapBuffers
  sleep 0.5
  swapBuffers
  letsDraw (mapM_ (uncurry vertex2f) (coords car)) yellow
  swapBuffers
  sleep 0.5
  swapBuffers
  letsDraw (mapM_ (uncurry vertex2f) (coords car)) magenta
  swapBuffers
  sleep 0.5
  blinking car

--Перехват wasd
keyboardOnCar :: [Car] -> IO [Car]
keyboardOnCar cars = do
  l <- getKey 65
  r <- getKey 68
  u <- getKey 87
  d <- getKey 83
  if l == Press then do
    sleep 0.15
    return (turnLeft cars)
  else if r == Press then do
      sleep 0.15
      return (turnRight cars)
  else if u == Press then do
      sleep 0.15
      return (turnUp cars)
  else if d == Press then do
      sleep 0.15
      return (turnDown cars)
  else return cars

--Далее - повороты
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

--Просмотр всех машин и перемещение нужной
moveCars :: [Car] -> [Car] -> Car -> Dir -> [Car]
moveCars _ [] _ _ = []
moveCars cars (c:cs) car dir = 
  if c == car then 
    (moveCar cars car dir):(moveCars cars cs car dir) 
  else c:(moveCars cars cs car dir)

--Перемещение carToMove на клетку в направлении direc
moveCar :: [Car] -> Car -> Dir -> Car
moveCar cars (carToMove@(Car d x y ln col)) direc 
  | direc == ToUp = if isEmpty cars (getAddCoordXY carToMove (0,(-1))) carToMove then (Car d x (y-1) ln col) else carToMove
  | direc == ToDown = if y+ln+1<=6 && isEmpty cars (getAddCoordXY carToMove (0,ln)) carToMove then (Car d x (y+1) ln col) else carToMove
  | direc == ToLeft = if isEmpty cars (getAddCoordXY carToMove ((-1),0)) carToMove then (Car d (x-1) y ln col) else carToMove
  | direc == ToRight = if x+ln+1<=6 && isEmpty cars (getAddCoordXY carToMove (ln,0)) carToMove then (Car d (x+1) y ln col) else carToMove

--Пустая ли клетка (xx,yy). carToMove нужно, чтобы не смотрели пересечения с выбраной машиной
isEmpty ::  [Car] -> (Integer,Integer) -> Car -> Bool
isEmpty [] _ _ = True
isEmpty ((c@(Car d x y ln _)):cs) (xx,yy) carToMove = 
  if inField (xx,yy) then do
    if c /= carToMove && ((d == 1 && x == xx && y <= yy && yy < y+ln) || 
      (d == 0 && y == yy && x <= xx && xx < x+ln)) then False
    else isEmpty cs (xx,yy) carToMove
  else False
  
--Находится ли клетка в поле
inField (x, y) = x >= 0 && x < 600 && y >= 0 && y < 600
  
--(не)Находит выбранную машину
findOrangeCar :: [Car] -> Maybe Car
findOrangeCar [] = Nothing
findOrangeCar (c:cs) = if (getColor c) == orange then 
    Just c
  else findOrangeCar cs
   
--По нажатию мыши производит пересмотр всех машин, их цветов
mouseOnCar :: [Car] -> IO [Car]
mouseOnCar cars = do
  mbl <- getMouseButton ButtonLeft
  if (mbl == Press) then do
    mpos <- get mousePos 
    updateCars cars mpos
  else return cars

--Если "попали" кликом на машину, возвращает измененные машины(цвет)
updateCars :: [Car] -> Position -> IO [Car]
updateCars cars p@(Position x y) = do
  let foundedCar = inCar p cars
  if isJust foundedCar then
    return (changeTheCar cars (fromJust foundedCar))
  else return cars

-- Меняет цвет выбранной машины (если попали)  
changeTheCar :: [Car] -> Car -> [Car]
changeTheCar [] c = []
changeTheCar (c1@(Car dir x y ln col):xs) c2@(Car dirC xC yC _ _) = 
  if dir == dirC && x == xC && y == yC then 
    (Car dir x y ln orange):(changeTheCar xs c2) else
  if dir == 0 && y == 2 then (Car dir x y ln green):(changeTheCar xs c2) 
  else (Car dir x y ln red):(changeTheCar xs c2) 

--Если кликнули на машину, вернули ее
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
  
--Рисуем машины, рисуем линию
display cars = do
  clear [ColorBuffer] 
  printCars cars
  color green
  fatline 598 200 598 300
  swapBuffers

--Рисование машин
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

yellow = Color4 (1::GLfloat) 1 0 1
cyan = Color4 (0::GLfloat) 1 1 1
magenta = Color4 (1::GLfloat) 0 1 1

--Рисование толстой линии
fatline :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
fatline ax ay bx by = renderPrimitive Lines $ do
  vertex2f ax ay
  vertex2f bx by
  vertex2f (ax-1) ay
  vertex2f (bx-1) by
  vertex2f (ax+1) ay
  vertex2f (bx+1) by
  vertex2f (ax+2) ay
  vertex2f (bx+2) by
  vertex2f (ax-2) ay
  vertex2f (bx-2) by

--get's
getColor (Car _ _ _ _ col) = col
getDirection (Car dir _ _ _ _) = dir
getAddCoordXY (Car _ x y _ _) (a,b) = (x+a,y+b)
getCarLength (Car _ _ _ ln _) = ln

--Само рисование
carCoord :: Car -> IO ()
carCoord c = 
  letsDraw (mapM_ (uncurry vertex2f) (coords c)) (getColor c)

--Преобразование клеток, в которых находится машина, в координаты окна
coords (c@(Car dir x y len _ ))
  | dir == 1 = [(toScale1 x, toScale1 y),(toScale2 (x+1), toScale1 y), 
							(toScale2 (x+1), toScale2 (y+len)), (toScale1 x, toScale2 (y+len))]
  | dir == 0 = [(toScale1 x, toScale1 y),(toScale2 (x+len), toScale1 y),
							(toScale2 (x+len), toScale2 (y+1)), (toScale1 x, toScale2 (y+1))]
  | otherwise = error "Create the Coords!"

--Как выше, но без преобразования в Fractional
coords2 (c@(Car dir x y len _ ))
  | dir == 1 = [(toScale11 x, toScale11 y),(toScale22 (x+1), toScale11 y), 
							(toScale22 (x+1), toScale22 (y+len)), (toScale11 x, toScale22 (y+len))]
  | dir == 0 = [(toScale11 x, toScale11 y),(toScale22 (x+len), toScale11 y),
							(toScale22 (x+len), toScale22 (y+1)), (toScale11 x, toScale22 (y+1))]
  | otherwise = error "Create the Coords!"

--Выбираем цвет машины, рисуем прямоугольник
letsDraw c col = do
  color col
  renderPrimitive Quads c  

--Из клетки в координату с отступом(между машинами)
toScale1 n = realToFrac(n * 100)+3
toScale2 n = realToFrac(n * 100)-3

toScale11 n = (n * 100)+3
toScale22 n = (n * 100)-3