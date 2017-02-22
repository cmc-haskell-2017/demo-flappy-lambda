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

-- | Обновить ворота игровой вселенной.
updateGates :: Float -> [Gate] -> [Gate]
updateGates _ [] = []
updateGates dt ((offset, height) : gates)
  | dx > pos  = updateGates dt' gates
  | otherwise = (offset - dx, height) : gates
  where
    pos = offset - screenLeft + gateWidth
    dx  = dt * speed
    dt' = dt - offset / speed

-- | Расстояние между воротами.
defaultOffset :: Offset
defaultOffset = 300

-- | Диапазон высот ворот.
gateHeightRange :: (Height, Height)
gateHeightRange = (-h, h)
  where
    h = (fromIntegral screenHeight - gateWidth) / 2

-- | Скорость движения игрока по вселенной (в пикселях в секунду).
speed :: Float
speed = 100

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

-- | Счёт.
type Score = Int

-- | Модель игровой вселенной.
data Universe = Universe
  { universeGates   :: [Gate]   -- ^ Ворота игровой вселенной.
  , universePlayer  :: Player   -- ^ Игрок.
  , universeScore   :: Score    -- ^ Счёт (кол-во успешно пройденных ворот).
  }

-- | Игрок — символ лямбда.
data Player = Player
  { playerHeight :: Height  -- ^ Положение игрока по вертикали.
  , playerSpeed  :: Float   -- ^ Скорость падения игрока.
  }

-- | Положение игрока по горизонтали.
playerOffset :: Offset
playerOffset = screenLeft + 200

-- | Ускорение свободного падения.
gravity :: Float
gravity = -1000

-- | Скорость после "подпрыгивания".
bumpSpeed :: Float
bumpSpeed = 400

-- | Инициализировать игровую вселенную, используя генератор случайных значений.
initUniverse :: StdGen -> Universe
initUniverse g = Universe
  { universeGates  = initGates g
  , universePlayer = initPlayer
  , universeScore  = 0
  }

-- | Начальное состояние игрока.
initPlayer :: Player
initPlayer = Player
  { playerHeight = 0
  , playerSpeed  = 0
  }

-- | Отобразить игровую вселенную.
drawUniverse :: Universe -> Picture
drawUniverse u = pictures
  [ drawGates  (universeGates u)
  , drawPlayer (universePlayer u)
  , drawScore  (universeScore u)
  ]

-- | Отобразить все ворота игровой вселенной, вмещающиеся в экран.
drawGates :: [Gate] -> Picture
drawGates = drawGates' screenRight
  where
    drawGates' r ((offset, height):gates)
      | offset < r + gateWidth = pictures
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

-- | Нарисовать игрока.
drawPlayer :: Player -> Picture
drawPlayer player = color orange (translate 0 (playerHeight player) drawLambda)
  where
    tilt = rotate (0.3 * (atan2 speed (playerSpeed player) * 180 / pi - 90))
    drawLambda = tilt (scale 0.03 0.03 (pictures
      [ polygon [ (-885, -770), (-525, -770), (50, 50), (-140, 370) ]
      , polygon [ (335, -865), (560, -510), (-35, 970), (-240, 675) ]
      , polygon [ (-35, 970), (-485, 970), (-485, 675), (-240, 675) ]
      , polygon [ (355, -855), (900, -690), (805, -435), (510, -510) ]
      ]))

-- | Нарисовать счёт в левом верхнем углу экрана.
drawScore :: Score -> Picture
drawScore score = translate (-w) h (scale 30 30 (pictures
  [ color white (polygon [ (0, 0), (0, -2), (6, -2), (6, 0) ])            -- белая рамка
  , color black (polygon [ (0, 0), (0, -1.9), (5.9, -1.9), (5.9, 0) ])    -- чёрные внутренности
  , translate 2 (-1.5) (scale 0.01 0.01 (color red (text (show score))))  -- красный счёт
  ]))
  where
    w = fromIntegral screenWidth  / 2
    h = fromIntegral screenHeight / 2

-- | Ширина стенок ворот.
gateWidth :: Float
gateWidth = 40

-- | Размер проёма ворот.
gateSize :: Float
gateSize = 150

-- | Положение правого края экрана.
screenRight :: Offset
screenRight = fromIntegral screenWidth / 2

-- | Положение левого края экрана.
screenLeft :: Offset
screenLeft = - fromIntegral screenWidth / 2

-- | Обработчик событий игры.
handleUniverse :: Event -> Universe -> Universe
handleUniverse (EventKey (SpecialKey KeySpace) Down _ _) = bumpPlayer
handleUniverse _ = id

-- | Подпрыгнуть (игроком), если можно.
bumpPlayer :: Universe -> Universe
bumpPlayer u = u
  { universePlayer = bump (universePlayer u)
  }
  where
    bump player
      | playerSpeed player < 0
          = player { playerSpeed = bumpSpeed }
      | otherwise = player

-- | Рассчитать абсолютное положение ворот.
absoluteGates :: [Gate] -> [Gate]
absoluteGates = go 0
  where
    go _ [] = []
    go s ((o, h) : gs) = (o + s, h) : go (s + o) gs

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse dt u = u
  { universeGates  = updateGates  dt (universeGates  u)
  , universePlayer = updatePlayer dt (universePlayer u)
  , universeScore  = universeScore u + scoreGates (universeGates u)
  }
  where
    scoreGates = length . takeWhile isPast . dropWhile wasPast . absoluteGates
    -- ворота окажутся позади игрока в этом кадре?
    isPast (offset, _) = offset + gateWidth / 2 - dt * speed < 0
    -- ворота уже были позади игрока в предыдущем кадре?
    wasPast (offset, _) = offset + gateWidth / 2 < 0

-- | Обновить состояние игрока.
updatePlayer :: Float -> Player -> Player
updatePlayer dt player = player
  { playerHeight = playerHeight player + dt * playerSpeed player
  , playerSpeed  = playerSpeed  player + dt * gravity
  }

