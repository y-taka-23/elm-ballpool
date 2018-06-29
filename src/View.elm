module View exposing (..)

import Color exposing (Color)
import Json.Decode as Json
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
import Mouse
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Events exposing (..)
import TypedSvg.Core exposing (..)
import TypedSvg.Types exposing (..)
import Model exposing (..)
import Style exposing (..)


view : Model -> Html Msg
view model =
    Html.div [ pageStyle ]
        [ display model
        , panel model
        ]


display : Model -> Html Msg
display model =
    let
        s =
            model.viewBox.scale

        tl =
            model.viewBox.topLeft

        br =
            model.viewBox.bottomRight
    in
        svg
            [ width <| px <| s.x * (br.x - tl.x)
            , height <| px <| s.y * (br.y - tl.y)
            , viewBox (tl.x) (tl.y) (br.x) (br.y)
            , onMouseDownOffset StartTmpLine
            , onMouseMoveOffset MoveTmpLine
            , onMouseUpOffset EndTmpLine
            , displayStyle
            ]
            -- Todo: better treatment
            (List.map toSvg model.objects ++ tmpLineToSvg model.tmpLine)


offset : Json.Decoder Mouse.Position
offset =
    Json.map2 Mouse.Position
        (Json.field "offsetX" Json.int)
        (Json.field "offsetY" Json.int)


onMouseDownOffset : (Mouse.Position -> msg) -> Attribute msg
onMouseDownOffset message =
    on "mousedown" <| Json.map message offset


onMouseUpOffset : (Mouse.Position -> msg) -> Attribute msg
onMouseUpOffset message =
    on "mouseup" <| Json.map message offset


onMouseMoveOffset : (Mouse.Position -> msg) -> Attribute msg
onMouseMoveOffset message =
    on "mousemove" <| Json.map message offset


panel : Model -> Html Msg
panel model =
    Html.div [ panelStyle ]
        (ballSlider model ++ gravitySlider model)


tmpLineToSvg : Maybe Line -> List (Svg msg)
tmpLineToSvg tl =
    case tl of
        Nothing ->
            []

        Just l ->
            [ line
                [ x1 (px l.from.x)
                , y1 (px l.from.y)
                , x2 (px l.to.x)
                , y2 (px l.to.y)
                , strokeWidth (px l.width)
                , stroke Color.darkGray
                ]
                []
            ]


toSvg : Object -> Svg Msg
toSvg o =
    case o.shape of
        CircleShape c ->
            circleToSvg o.color c

        LineShape l ->
            lineToSvg o.id o.color l

        AABBShape b ->
            aabbToSvg o.color b


circleToSvg : Color -> Circle -> Svg msg
circleToSvg col c =
    circle
        [ cx (px c.center.x)
        , cy (px c.center.y)
        , r (px c.radius)
        , fill (Fill col)
        ]
        []


lineToSvg : ObjectId -> Color -> Line -> Svg Msg
lineToSvg i col l =
    line
        [ x1 (px l.from.x)
        , y1 (px l.from.y)
        , x2 (px l.to.x)
        , y2 (px l.to.y)
        , strokeWidth (px l.width)
        , stroke col
        , onClick (DeleteLine i)
        , TypedSvg.Attributes.cursor CursorPointer
        ]
        []


aabbToSvg : Color -> AABB -> Svg msg
aabbToSvg col b =
    rect
        [ x (px b.topLeft.x)
        , y (px b.topLeft.y)
        , width (px (b.bottomRight.x - b.topLeft.x))
        , height (px (b.bottomRight.y - b.topLeft.y))
        , fill (Fill col)
        ]
        []


ballSlider : Model -> List (Html Msg)
ballSlider model =
    [ Html.label [ Html.for "balls" ] [ Html.text "Balls" ]
    , Html.input
        [ Html.id "balls"
        , Html.type_ "range"
        , Html.min "0"
        , Html.max "30"
        , Html.step "1"
        , Html.value <| toString model.balls
        , Html.onInput (String.toInt >> Result.withDefault 0 >> SetBalls)
        ]
        []
    ]


gravitySlider : Model -> List (Html Msg)
gravitySlider model =
    [ Html.label [ Html.for "gravity" ] [ Html.text "Gravity" ]
    , Html.input
        [ Html.id "gravity"
        , Html.type_ "range"
        , Html.min "0"
        , Html.max "0.01"
        , Html.step "0.001"
        , Html.value <| toString model.gravity
        , Html.onInput (String.toFloat >> Result.withDefault 0 >> SetGravity)
        ]
        []
    ]
