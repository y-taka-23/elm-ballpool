module Vector exposing (..)


type alias Vector =
    { x : Float
    , y : Float
    }


(.+) : Vector -> Vector -> Vector
(.+) v1 v2 =
    { x = v1.x + v2.x, y = v1.y + v2.y }


(.-) : Vector -> Vector -> Vector
(.-) v1 v2 =
    { x = v1.x - v2.x, y = v1.y - v2.y }


(:*) : Float -> Vector -> Vector
(:*) c v =
    { x = c * v.x, y = c * v.y }


(.*) : Vector -> Vector -> Float
(.*) v1 v2 =
    v1.x * v2.x + v1.y * v2.y


(.><) : Vector -> Vector -> Float
(.><) v1 v2 =
    v1.x * v2.y - v1.y * v2.x


normSq : Vector -> Float
normSq v =
    v .* v


norm : Vector -> Float
norm v =
    sqrt <| normSq v


normalize : Vector -> Vector
normalize v =
    if norm v /= 0 then
        (1 / norm v) :* v
    else
        { x = 1, y = 0 }
