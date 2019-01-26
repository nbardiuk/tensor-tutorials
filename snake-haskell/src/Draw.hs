module Draw (drawGame, toCoord) where

import           Control.Monad                  ( when )
import           Data.Foldable                  ( traverse_ )
import           Game
import           GHC.Word
import           SDL

type Color = V4 Word8

snakeColor :: Color
snakeColor = V4 0 204 0 255

foodColor :: Color
foodColor = V4 204 0 0 255

borderColor :: Color
borderColor = V4 0 0 0 255

gameoverColor :: Color
gameoverColor = V4 230 0 0 128

backColor :: Color
backColor = V4 128 128 128 255

blockSize :: Int
blockSize = 25

toCoord :: Int -> Int
toCoord = (* blockSize)

drawGame :: Renderer -> Game -> IO ()
drawGame r game = do
  rendererDrawColor r $= backColor >> clear r
  drawSnake r $ snake game
  when (foodExists game) $ drawBlock r foodColor (food game)
  drawBorder r game
  when (gameover game) $ drawRectangle r gameoverColor (Rect (Block 0 0) (size game))
  present r

drawBorder :: Renderer -> Game -> IO ()
drawBorder r game =
  let height = h $ size game
      width  = w $ size game
  in  traverse_
        (drawRectangle r borderColor)
        [ Rect (Block 0 0)            (Size width 1)
        , Rect (Block 0 (height - 1)) (Size width 1)
        , Rect (Block 0 0)            (Size 1 height)
        , Rect (Block (height - 1) 0) (Size 1 height)
        ]

drawSnake :: Renderer -> Snake -> IO ()
drawSnake r = traverse_ (drawBlock r snakeColor) . body

drawBlock :: Renderer -> Color -> Block -> IO ()
drawBlock renderer color block =
  drawRectangle renderer color (Rect block (Size 1 1))

drawRectangle :: Renderer -> Color -> Rect -> IO ()
drawRectangle renderer color (Rect (Block xr yr) (Size wr hr)) = do
  rendererDrawColor renderer $= color
  rendererDrawBlendMode renderer $= BlendAlphaBlend
  fillRect renderer $ Just $ Rectangle
    (P (V2 (toEnum $ toCoord xr) (toEnum $ toCoord yr)))
    (V2 (toEnum $ toCoord wr) (toEnum $ toCoord hr))
