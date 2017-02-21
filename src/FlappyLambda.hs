module FlappyLambda where

import System.Random
import Graphics.Gloss.Interface.Pure.Game

type Height = Float
type Offset = Float
type Gate   = (Offset, Height)

-- | Инициализировать одни ворота.
initGate :: Height -> Gate
initGate h = (defaultOffset, h)

-- | Инициализировать случайный бесконечный
-- список ворот для игровой вселенной.
initGates :: StdGen -> [Gate]
initGates g = map initGate
  (randomRs gateHeightRange g)

-- | Расстояние между воротами.
defaultOffset :: Offset
defaultOffset = 200

-- | Диапазон высот ворот.
gateHeightRange :: (Height, Height)
gateHeightRange = (-200, 200)

-- | Запустить игру «Flappy Lambda».
runFlappyLambda :: IO ()
runFlappyLambda = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) drawUniverse handleUniverse updateUniverse
  where
    display = InWindow "Flappy Lambda" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Ширина экрана.
screenWidth :: Int
screenWidth = 800

-- | Высота экрана.
screenHeight :: Int
screenHeight = 450

-- | Модель игровой вселенной.
data Universe = Universe
  { universeGates :: [Gate]   -- ^ Ворота игровой вселенной.
  }

-- | Инициализировать игровую вселенную, используя генератор случайных значений.
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { universeGates = initGates g }

-- | Отобразить игровую вселенную.
drawUniverse :: Universe -> Picture
drawUniverse u = drawGates (universeGates u)

-- | Отобразить все ворота игровой вселенной, вмещающиеся в экран.
drawGates :: [Gate] -> Picture
drawGates = drawGates' screenRight
  where
    drawGates' r ((offset, height):gates)
      | offset < r = pictures
          [ drawGate (offset, height)
          , translate offset 0 (drawGates' (r - offset) gates)
          ]
      | otherwise = blank

-- | Нарисовать одни ворота.
drawGate :: Gate -> Picture
drawGate (offset, height) = color white (translate offset height gate)
  where
    gate = pictures
      [ polygon [(-w,  s), (w,  s), (w,  s + h), (-w,  s + h)]
      , polygon [(-w, -s), (w, -s), (w, -s - h), (-w, -s - h)]
      ]
    w = gateWidth / 2
    s = gateSize / 2
    h = fromIntegral screenHeight

-- | Ширина стенок ворот.
gateWidth :: Float
gateWidth = 20

-- | Размер проёма ворот.
gateSize :: Float
gateSize = 100

-- | Положение правого края экрана.
screenRight :: Offset
screenRight = 400

-- | Обработчик событий игры.
handleUniverse :: Event -> Universe -> Universe
handleUniverse _ = id

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse _ = id
