module Update exposing (..)

import Mouse
import Random
import Time exposing (Time)
import Model exposing (..)
import Vector exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewFrame diff ->
            ( { model
                | objects =
                    pipeline model.viewBox model.gravity diff model.objects
              }
            , Cmd.none
            )

        StartTmpLine pos ->
            case model.tmpLine of
                Nothing ->
                    let
                        vector =
                            scale model.viewBox pos
                    in
                        ( { model | tmpLine = Just <| mkTmpLine vector vector }
                        , Cmd.none
                        )

                Just _ ->
                    ( model, Cmd.none )

        MoveTmpLine pos ->
            case model.tmpLine of
                Nothing ->
                    ( model, Cmd.none )

                Just l ->
                    let
                        newTmpLine =
                            { l | to = scale model.viewBox pos }
                    in
                        ( { model | tmpLine = Just newTmpLine }, Cmd.none )

        EndTmpLine pos ->
            case model.tmpLine of
                Nothing ->
                    ( model, Cmd.none )

                Just l ->
                    if l.from /= l.to then
                        ( { model
                            | objects =
                                lineToObject model.nextId l :: model.objects
                            , tmpLine = Nothing
                            , nextId = model.nextId + 1
                          }
                        , Cmd.none
                        )
                    else
                        ( { model | tmpLine = Nothing }, Cmd.none )

        DeleteLine i ->
            ( { model | objects = List.filter (\o -> o.id /= i) model.objects }
            , Cmd.none
            )

        SetBalls n ->
            let
                diff =
                    countBalls model.objects - n
            in
                if diff >= 0 then
                    ( { model
                        | objects = dropBalls diff model.objects
                        , balls = n
                      }
                    , Cmd.none
                    )
                else
                    ( { model | balls = n }
                    , Random.generate SupplyBalls (genBalls -diff)
                    )

        SupplyBalls newBalls ->
            ( { model | objects = newBalls ++ model.objects }, Cmd.none )

        SetGravity grav ->
            ( { model | gravity = grav }, Cmd.none )


pipeline : ViewBox -> Float -> Time -> List Object -> List Object
pipeline vb grav diff =
    List.map (applyGravity grav)
        >> resolveCollisions
        >> List.map (displace << updatePosition diff)
        >> List.map (wrap vb)


applyGravity : Float -> Object -> Object
applyGravity grav o =
    case o.mass of
        Finite _ ->
            { o | velocity = o.velocity .+ { x = 0, y = grav } }

        Infinite ->
            o


detectCollision : Object -> Object -> Maybe Manifold
detectCollision o1 o2 =
    case ( o1.shape, o2.shape ) of
        ( CircleShape c1, CircleShape c2 ) ->
            detectCollisionCtoC c1 c2

        ( LineShape l, CircleShape c ) ->
            detectCollisionLtoC l c

        ( CircleShape c, LineShape l ) ->
            case detectCollisionLtoC l c of
                Nothing ->
                    Nothing

                Just mf ->
                    Just { mf | normal = -1 :* mf.normal }

        ( AABBShape b, CircleShape c ) ->
            detectCollisionBtoC b c

        ( CircleShape c, AABBShape b ) ->
            case detectCollisionBtoC b c of
                Nothing ->
                    Nothing

                Just mf ->
                    Just { mf | normal = -1 :* mf.normal }

        -- actually never happens
        ( _, _ ) ->
            Nothing


detectCollisionCtoC : Circle -> Circle -> Maybe Manifold
detectCollisionCtoC c1 c2 =
    let
        relPos =
            c2.center .- c1.center

        radii =
            c1.radius + c2.radius
    in
        if normSq relPos > radii ^ 2 then
            Nothing
        else if normSq relPos == 0 then
            Just { penetration = c1.radius, normal = { x = 1, y = 0 } }
        else
            -- Todo: avoid to calculate sqrt twise
            Just
                { penetration = radii - norm relPos
                , normal = normalize relPos
                }


detectCollisionLtoC : Line -> Circle -> Maybe Manifold
detectCollisionLtoC l c =
    let
        fromToCenter =
            c.center .- l.from

        toToCenter =
            c.center .- l.to

        fromToTo =
            l.to .- l.from

        innerProd =
            fromToCenter .* fromToTo
    in
        if innerProd < 0 then
            if c.radius ^ 2 > normSq fromToCenter then
                -- Todo: avoid to calculate sqrt twise
                Just
                    { penetration = c.radius - norm fromToCenter
                    , normal = normalize fromToCenter
                    }
            else
                Nothing
        else if normSq fromToTo < innerProd then
            if c.radius ^ 2 > normSq toToCenter then
                Just
                    { penetration = c.radius - norm toToCenter
                    , normal = normalize toToCenter
                    }
            else
                Nothing
        else
            let
                dist =
                    fromToTo .>< fromToCenter / (norm fromToTo)

                penetration =
                    c.radius + l.width / 2 - abs dist
            in
                if penetration > 0 && dist >= 0 then
                    Just
                        { penetration = penetration
                        , normal =
                            normalize { x = -fromToTo.y, y = fromToTo.x }
                        }
                else if penetration > 0 && dist < 0 then
                    Just
                        { penetration = penetration
                        , normal =
                            normalize { x = fromToTo.y, y = -fromToTo.x }
                        }
                else
                    Nothing


detectCollisionBtoC : AABB -> Circle -> Maybe Manifold
detectCollisionBtoC b c =
    let
        pointx =
            if c.center.x < b.topLeft.x then
                b.topLeft.x
            else if b.bottomRight.x < c.center.x then
                b.bottomRight.x
            else
                c.center.x

        pointy =
            if c.center.y < b.topLeft.y then
                b.topLeft.y
            else if b.bottomRight.y < c.center.y then
                b.bottomRight.y
            else
                c.center.y
    in
        -- Assume that the point is out of the box
        detectCollisionWithPoint { x = pointx, y = pointy } c


detectCollisionWithPoint : Vector -> Circle -> Maybe Manifold
detectCollisionWithPoint p c =
    let
        cornerToCenter =
            c.center .- p
    in
        if c.radius ^ 2 > normSq cornerToCenter then
            Just
                { penetration = c.radius - norm cornerToCenter
                , normal = normalize cornerToCenter
                }
        else
            Nothing


resolveCollision : Object -> Object -> ( Object, Object )
resolveCollision o1 o2 =
    -- the direction of the normal vector should be: o1 -> o2
    case detectCollision o1 o2 of
        Nothing ->
            ( o1, o2 )

        Just mf ->
            let
                e =
                    Basics.min o1.restitution o2.restitution

                ( weight1, weight2 ) =
                    weight o1.mass o2.mass

                newPosition1 =
                    o1.position .- ((weight2 * mf.penetration) :* mf.normal)

                newPosition2 =
                    o2.position .+ ((weight1 * mf.penetration) :* mf.normal)

                proj =
                    (o1.velocity .- o2.velocity) .* mf.normal

                newVelocity1 =
                    o1.velocity .- ((weight2 * (e + 1) * proj) :* mf.normal)

                newVelocity2 =
                    o2.velocity .+ ((weight1 * (e + 1) * proj) :* mf.normal)
            in
                ( { o1 | position = newPosition1, velocity = newVelocity1 }
                , { o2 | position = newPosition2, velocity = newVelocity2 }
                )


weight : Mass -> Mass -> ( Float, Float )
weight m1 m2 =
    case ( m1, m2 ) of
        -- actually never happens
        ( Infinite, Infinite ) ->
            ( 0.5, 0.5 )

        ( Finite x, Infinite ) ->
            ( 0, 1 )

        ( Infinite, Finite y ) ->
            ( 1, 0 )

        ( Finite x, Finite y ) ->
            ( x / (x + y), y / (x + y) )


updatePosition : Time -> Object -> Object
updatePosition diff o =
    { o | position = o.position .+ (diff :* o.velocity) }


displace : Object -> Object
displace o =
    case o.shape of
        CircleShape c ->
            { o | shape = CircleShape <| displaceCircle o.position c }

        LineShape l ->
            { o | shape = LineShape <| displaceLine o.position l }

        AABBShape b ->
            { o | shape = AABBShape <| displaceAABB o.position b }


displaceCircle : Vector -> Circle -> Circle
displaceCircle newPosition c =
    { c | center = newPosition }


displaceLine : Vector -> Line -> Line
displaceLine newPosition l =
    { l | from = newPosition, to = (l.to .- l.from) .+ newPosition }


displaceAABB : Vector -> AABB -> AABB
displaceAABB newPosition b =
    { b
        | topLeft = newPosition
        , bottomRight = (b.bottomRight .- b.topLeft) .+ newPosition
    }


resolveCollisions : List Object -> List Object
resolveCollisions =
    roundRobin resolveCollision


roundRobin : (a -> a -> ( a, a )) -> List a -> List a
roundRobin interact list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                ( y, ys ) =
                    goThrough interact x xs
            in
                y :: roundRobin interact ys


goThrough : (a -> a -> ( a, a )) -> a -> List a -> ( a, List a )
goThrough interact x xs =
    case xs of
        [] ->
            ( x, [] )

        y :: ys ->
            let
                ( z, zs ) =
                    goThrough interact x ys

                ( z0, y0 ) =
                    interact z y
            in
                ( z0, y0 :: zs )


wrap : ViewBox -> Object -> Object
wrap vb o =
    let
        newX =
            if o.position.x >= vb.bottomRight.x then
                vb.topLeft.x
            else
                o.position.x

        newY =
            if o.position.y >= vb.bottomRight.y + 100 then
                vb.topLeft.y - 100
            else
                o.position.y

        newV =
            if o.position.y >= vb.bottomRight.y + 100 then
                0.1 :* o.velocity
            else
                o.velocity
    in
        { o | position = { x = newX, y = newY }, velocity = newV }


scale : ViewBox -> Mouse.Position -> Vector
scale vb pos =
    { x = toFloat pos.x / vb.scale.x + vb.topLeft.x
    , y = toFloat pos.y / vb.scale.y + vb.topLeft.y
    }


countBalls : List Object -> Int
countBalls objects =
    case objects of
        [] ->
            0

        o :: objs ->
            case o.shape of
                CircleShape _ ->
                    1 + countBalls objs

                _ ->
                    countBalls objs


dropBalls : Int -> List Object -> List Object
dropBalls n objects =
    case ( n, objects ) of
        ( _, [] ) ->
            []

        ( 0, objs ) ->
            objs

        ( n, o :: objs ) ->
            case o.shape of
                CircleShape _ ->
                    dropBalls (n - 1) objs

                _ ->
                    o :: dropBalls n objs
