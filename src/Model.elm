module Model exposing (..)

import Color exposing (Color)
import Mouse
import Random exposing (Generator)
import Random.Extra as Random
import Time exposing (Time)
import Vector exposing (..)


type Msg
    = NewFrame Time
    | StartTmpLine Mouse.Position
    | MoveTmpLine Mouse.Position
    | EndTmpLine Mouse.Position
    | DeleteLine ObjectId
    | SetBalls Int
    | SupplyBalls (List Object)
    | SetGravity Float


type alias Model =
    { objects : List Object
    , tmpLine : Maybe Line
    , nextId : ObjectId
    , balls : Int
    , gravity : Float
    , viewBox : ViewBox
    }


type alias Object =
    { id : ObjectId
    , position : Vector -- needs smart constructors
    , shape : Shape
    , color : Color
    , mass : Mass
    , restitution : Float
    , velocity : Vector
    }


type alias ObjectId =
    Int


type alias ViewBox =
    { topLeft : Vector
    , bottomRight : Vector
    , scale : Vector
    }


type Shape
    = CircleShape Circle
    | LineShape Line
    | AABBShape AABB


type Mass
    = Finite Float
    | Infinite


type alias Circle =
    { center : Vector
    , radius : Float
    }


type alias Line =
    { from : Vector
    , to : Vector
    , width : Float
    }


type alias AABB =
    { topLeft : Vector
    , bottomRight : Vector
    }


type alias Manifold =
    { penetration : Float
    , normal : Vector
    }


init : ( Model, Cmd Msg )
init =
    ( { objects = [ wallL, wallR ]
      , tmpLine = Nothing
      , nextId = 1
      , balls = 0
      , gravity = 0.01
      , viewBox =
            { topLeft = { x = 0, y = 0 }
            , bottomRight = { x = 300, y = 300 }
            , scale = { x = 1, y = 1 }
            }
      }
    , Cmd.none
    )


wallL : Object
wallL =
    { id = 0
    , position = { x = -10, y = -200 }
    , shape =
        AABBShape
            { topLeft = { x = -10, y = -200 }
            , bottomRight = { x = 10, y = 500 }
            }
    , color = Color.darkGray
    , mass = Infinite
    , restitution = 0.5
    , velocity = { x = 0, y = 0 }
    }


wallR : Object
wallR =
    { id = 0
    , position = { x = 290, y = -200 }
    , shape =
        AABBShape
            { topLeft = { x = 290, y = -200 }
            , bottomRight = { x = 310, y = 500 }
            }
    , color = Color.darkGray
    , mass = Infinite
    , restitution = 0.5
    , velocity = { x = 0, y = 0 }
    }


lineToObject : ObjectId -> Line -> Object
lineToObject i l =
    { id = i
    , position = l.from
    , shape = LineShape l
    , color = Color.green
    , mass = Infinite
    , restitution = 1
    , velocity = { x = 0, y = 0 }
    }


genBalls : Int -> Generator (List Object)
genBalls n =
    Random.list n genBall


genBall : Generator Object
genBall =
    Random.map3 mkBall genScatter genSize genColor


genScatter : Generator Vector
genScatter =
    Random.map2 Vector (Random.float -50 50) (Random.float -50 50)


genSize : Generator Float
genSize =
    Random.float 8 15


genColor : Generator Color
genColor =
    Random.sample [ Color.orange, Color.lightOrange, Color.darkOrange ]
        |> Random.map (Maybe.withDefault Color.orange)


mkBall : Vector -> Float -> Color -> Object
mkBall scatter size col =
    let
        pos =
            { x = 150, y = 20 } .+ scatter
    in
        { id = 0
        , position = pos
        , shape = CircleShape { center = pos, radius = size }
        , color = col
        , mass = Finite (size ^ 2)
        , restitution = 0.3
        , velocity = { x = 0, y = 0 }
        }


mkTmpLine : Vector -> Vector -> Line
mkTmpLine f t =
    { from = f, to = t, width = 10 }
