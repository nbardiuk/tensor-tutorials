module Game where

import           System.Random
import           Data.List.NonEmpty             ( NonEmpty((:|)) )
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Maybe                     ( fromMaybe
                                                , maybeToList
                                                , isNothing
                                                )

movingPeriod :: Double
movingPeriod = 0.1

restartTime :: Double
restartTime = 1.0

data Game = Game
  { snake::Snake
  , foodExists::Bool
  , food::Block
  , size::Size
  , gameover::Bool
  , waitingTime::Double
  , rng::StdGen
  }

newGame :: Int -> Int -> StdGen -> Game
newGame width height =
  Game (newSnake 2 2) True (Block 6 4) (Size width height) False 0.0

restart :: Game -> Game
restart g = newGame (w $ size g) (h $ size g) (rng g)

data Key = KUp | KDown | KLeft | KRight

update :: Game -> Double -> Game
update game dt =
  let g = game { waitingTime = waitingTime game + dt }
  in  if gameover g
        then if waitingTime g > restartTime then restart g else g
        else
          let g2 = if not (foodExists g) then addFood g else g
          in  if waitingTime g2 > movingPeriod
                then updateSnake g2 Nothing
                else g2

addFood :: Game -> Game
addFood game =
  let width    = w $ size game
      height   = h $ size game
      (nx, r1) = randomR (1, width - 2) $ rng game
      (ny, r2) = randomR (1, height - 2) r1
      fd       = Block nx ny
  in  if overlapTail fd (snake game)
        then addFood $ game { rng = r2 }
        else game { rng = r2, food = fd, foodExists = True }

keyPressed :: Game -> Maybe Key -> Game
keyPressed game mkey =
  let mdir = keyToDir <$> mkey
  in  if isNothing mdir
           || gameover game
           || mdir
           == (Just $ opposite $ direction $ snake game)
        then game
        else updateSnake game mdir

updateSnake :: Game -> Maybe Direction -> Game
updateSnake game mdir =
  let alive = isSnakeAlive game mdir
      snk   = snake game
  in  (if alive then checkEating else id) game
        { waitingTime = 0.0
        , gameover    = not alive || gameover game
        , snake       = (if alive then moveForward mdir else id) snk
        }

checkEating :: Game -> Game
checkEating game =
  let snk = snake game
      hd  = headPosition snk
  in  if foodExists game && food game == hd
        then game { foodExists = False, snake = restoreTail snk }
        else game

isSnakeAlive :: Game -> Maybe Direction -> Bool
isSnakeAlive game mdir =
  let sn  = snake game
      nxt = nextHead sn mdir
  in  not (overlapTail nxt sn) && nxt `inside` Rect (Block 0 0) (size game)

inside :: Block -> Rect -> Bool
inside (Block xo yo) (Rect (Block xr yr) (Size wr hr)) =
  xo > xr && yo > yr && xo < wr - 1 && yo < hr - 1

keyToDir :: Key -> Direction
keyToDir KUp    = U
keyToDir KDown  = D
keyToDir KLeft  = L
keyToDir KRight = R

data Direction = U | D | L | R deriving (Eq, Show)
opposite :: Direction -> Direction
opposite U = D
opposite D = U
opposite L = R
opposite R = L

data Block = Block {x::Int, y::Int} deriving (Eq)
data Size = Size {w::Int, h::Int} deriving (Eq)
data Rect = Rect {b::Block, s::Size} deriving (Eq)
data Snake = Snake {direction::Direction, body::NonEmpty Block, prevTail::Maybe Block}

newSnake :: Int -> Int -> Snake
newSnake xt yt =
  let bd = Block (xt + 2) yt :| [Block (xt + 1) yt, Block xt yt]
  in  Snake R bd Nothing

headPosition :: Snake -> Block
headPosition = NonEmpty.head . body

moveForward :: Maybe Direction -> Snake -> Snake
moveForward mdir snk =
  let dir = fromMaybe (direction snk) mdir
      new = moveBlock dir $ headPosition snk
      tl  = NonEmpty.last (body snk)
      bd  = new :| NonEmpty.init (body snk)
  in  Snake dir bd (Just tl)

nextHead :: Snake -> Maybe Direction -> Block
nextHead snk mdir =
  let dir = fromMaybe (direction snk) mdir in moveBlock dir $ headPosition snk

restoreTail :: Snake -> Snake
restoreTail snk =
  let (hd :| r) = body snk
      tl        = maybeToList $ prevTail snk
  in  snk { body = hd :| r ++ tl }

overlapTail :: Block -> Snake -> Bool
overlapTail block = elem block . NonEmpty.init . body

moveBlock :: Direction -> Block -> Block
moveBlock U bl = bl { y = y bl - 1 }
moveBlock D bl = bl { y = y bl + 1 }
moveBlock L bl = bl { x = x bl - 1 }
moveBlock R bl = bl { x = x bl + 1 }
