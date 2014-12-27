-- See this document for more information on making Pong:
-- http://elm-lang.org/blog/Pong.elm
import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal
import Signal (..)
import Text
import Char
import Time (..)
import Window

-- SIGNALS

main = view <~ Window.dimensions ~ gameState

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input

delta =
  Signal.map inSeconds (fps 35)

keyPressed : Char -> Signal Bool
keyPressed key =
    Char.toCode key |> Keyboard.isDown


input : Signal Input
input =
  Signal.sampleOn delta <|
    Input <~ Keyboard.space
      ~ (keyPressed 'r')
      ~ (keyPressed 'p')
      ~ (Signal.map .y Keyboard.wasd)
      ~ (Signal.map .y Keyboard.arrows)
      ~ delta

-- MODEL

(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (300, 200)

type State = Play | Pause

type alias Ball = {
    x: Float,
    y: Float,
    vx: Float,
    vy: Float
}

type alias Player = {
    x: Float,
    y: Float,
    vx: Float,
    vy: Float,
    score: Int
}

type alias Game = {
    state: State,
    ball: Ball,
    player1: Player,
    player2: Player
}


player : Float -> Player
player initialX =
  { x = initialX
  , y = 0
  , vx = 0
  , vy = 0
  , score = 0
  }

defaultGame : Game
defaultGame =
  { state   = Pause
  , ball    = { x = 0, y = 0, vx = 200, vy = 200 }
  , player1 = player (20 - halfWidth)
  , player2 = player (halfWidth-20)
  }


type alias Input = {
    space : Bool,
    reset : Bool,
    pause : Bool,
    dir1 : Int,
    dir2 : Int,
    delta : Time
}

-- UPDATE

update : Input -> Game -> Game
update {space, reset, pause, dir1, dir2, delta} ({state, ball, player1, player2} as game) =
  let score1 = if ball.x >  halfWidth then 1 else 0
      score2 = if ball.x < -halfWidth then 1 else 0

      newState =
        if  | space            -> Play
            | pause            -> Pause
            | score1 /= score2 -> Pause
            | otherwise        -> state

      newBall =
        if state == Pause
            then ball
            else updateBall delta ball player1 player2

  in
      if | reset -> defaultGame
         | otherwise -> { game |
          state <- newState,
          ball <- newBall,
          player1 <- updatePlayer delta dir1 score1 player1,
          player2 <- updatePlayer delta dir2 score2 player2
      }


--resetGame : Game -> Game
--resetGame game = game <~ defaultGame

updateBall : Time -> Ball -> Player -> Player -> Ball
updateBall t ({x, y, vx, vy} as ball) p1 p2 =
  if not (ball.x |> near 0 halfWidth)
    then { ball | x <- 0, y <- 0 }
    else physicsUpdate t
            { ball |
                vx <- stepV vx (ball `within` p1) (ball `within` p2),
                vy <- stepV vy (y < 7-halfHeight) (y > halfHeight-7)
            }


updatePlayer : Time -> Int -> Int -> Player -> Player
updatePlayer t dir points player =
  let player1 = physicsUpdate  t { player | vy <- toFloat dir * 200 }
  in
      { player1 |
          y <- clamp (22 - halfHeight) (halfHeight - 22) player1.y,
          score <- player.score + points
      }


physicsUpdate t ({x, y, vx, vy} as obj) =
  { obj |
      x <- x + vx * t,
      y <- y + vy * t
  }

near k c n =
    n >= k-c && n <= k+c

within ball paddle =
    near paddle.x 8 ball.x && near paddle.y 20 ball.y


stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v


-- VIEW

view : (Int,Int) -> Game -> Element
view (w, h) {state, ball, player1, player2} =
  let scores : Element
      scores = txt (Text.height 50) (toString player1.score ++ "  " ++ toString player2.score)
  in
      container w h middle <|
      collage gameWidth gameHeight
        [ rect gameWidth gameHeight
            |> filled pongGreen
        , verticalLine gameHeight
            |> traced (dashed red)
        , oval 15 15
            |> make ball
        , rect 10 40
            |> make player1
        , rect 10 40
            |> make player2
        , toForm scores
            |> move (0, gameHeight/2 - 40)
        , toForm (statusMessage state)
            |> move (0, 40 - gameHeight/2)
        ]

statusMessage state =
    case state of
        Play    -> txt identity ""
        Pause   -> txt identity pauseMessage

verticalLine height =
     path [(0, height), (0, -height)]

pongGreen = rgb 60 100 60
textGreen = rgb 160 200 160
txt f = Text.fromString >> Text.color textGreen >> Text.monospace >> f >> Text.leftAligned
pauseMessage = "SPACE to start, P to pause, R to reset, WS and &uarr;&darr; to move"

make obj shape =
    shape
      |> filled white
      |> move (obj.x,obj.y)

