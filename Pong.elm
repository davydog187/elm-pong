-- See this document for more information on making Pong:
-- http://elm-lang.org/blog/Pong.elm
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Signal exposing (..)
import Text
import Char
import Time exposing (..)
import Window

-- SIGNALS

main =  map2 view Window.dimensions gameState

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input

delta : Signal Float
delta =
  Signal.map inSeconds (fps 35)

keyPressed : Char -> Signal Bool
keyPressed key =
    Char.toCode key |> Keyboard.isDown


input : Signal Input
input =
  Signal.sampleOn delta <|
    map5 Input Keyboard.space
               (keyPressed 'r')
               (keyPressed 'p')
               (Signal.map .y Keyboard.arrows)
               delta

-- MODEL

(gameWidth, gameHeight) = (600, 400)
(halfWidth, halfHeight) = (gameWidth / 2, gameHeight / 2)

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
  , player2 = player (halfWidth - 20)
  }


type alias Input = {
    space : Bool,
    reset : Bool,
    pause : Bool,
    dir : Int,
    delta : Time
}

-- UPDATE

update : Input -> Game -> Game
update {space, reset, pause, dir, delta} ({state, ball, player1, player2} as game) =
  let score1 = if ball.x >  halfWidth then 1 else 0
      score2 = if ball.x < -halfWidth then 1 else 0

      newState =
        if  space then Play 
        else if (pause) then Pause 
        else if (score1 /= score2) then Pause 
        else state

      newBall =
        if state == Pause
            then ball
            else updateBall delta ball player1 player2

  in
      if reset
         then defaultGame
         else { game |
                  state     = newState,
                  ball      = newBall,
                  player1   = updatePlayer delta dir score1 player1,
                  player2   = updateComputer newBall score2 player2
              }

updateBall : Time -> Ball -> Player -> Player -> Ball
updateBall t ({x, y, vx, vy} as ball) p1 p2 =
  if not (ball.x |> near 0 halfWidth)
    then { ball | x = 0, y = 0 }
    else physicsUpdate t
            { ball |
                vx = stepV vx (ball `within` p1) (ball `within` p2),
                vy = stepV vy (y < 7-halfHeight) (y > halfHeight-7)
            }


updatePlayer : Time -> Int -> Int -> Player -> Player
updatePlayer t dir points player =
  let player1 = physicsUpdate  t { player | vy = toFloat dir * 200 }
  in
      { player1 |
          y = clamp (22 - halfHeight) (halfHeight - 22) player1.y,
          score = player.score + points
      }

updateComputer : Ball -> Int -> Player -> Player
updateComputer ball points player =
    { player |
        y = clamp (22 - halfHeight) (halfHeight - 22) ball.y,
        score = player.score + points
    }

physicsUpdate t ({x, y, vx, vy} as obj) =
  { obj |
      x = x + vx * t,
      y = y + vy * t
  }

near : Float -> Float -> Float -> Bool
near k c n =
    n >= k-c && n <= k+c

within ball paddle =
    near paddle.x 8 ball.x && near paddle.y 20 ball.y


stepV v lowerCollision upperCollision =
  if lowerCollision then abs v
  else if upperCollision then 0 - abs v
  else v


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
txt f = Text.fromString >> Text.color textGreen >> Text.monospace >> f >> leftAligned
pauseMessage = "SPACE to start, P to pause, R to reset, WS and &uarr;&darr; to move"

make obj shape =
    shape
      |> filled white
      |> move (obj.x,obj.y)

