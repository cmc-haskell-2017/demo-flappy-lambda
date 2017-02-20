module FlappyLambda where

import System.Random
import Graphics.Gloss.Interface.Pure.Game

-- | Запустить игру «Flappy Lambda».
runFlappyLambda :: IO ()
runFlappyLambda = do
  g <- newStdGen
  play display bgColor fps (initUniverse g) drawUniverse handleUniverse updateUniverse
  where
    display = InWindow "Flappy Lambda" (800, 450) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Модель игровой вселенной.
data Universe = Universe

-- | Инициализировать игровую вселенную, используя генератор случайных значений.
initUniverse :: StdGen -> Universe
initUniverse _ = Universe

-- | Отобразить игровую вселенную.
drawUniverse :: Universe -> Picture
drawUniverse _ = blank

-- | Обработчик событий игры.
handleUniverse :: Event -> Universe -> Universe
handleUniverse _ = id

-- | Обновить состояние игровой вселенной.
updateUniverse :: Float -> Universe -> Universe
updateUniverse _ = id
