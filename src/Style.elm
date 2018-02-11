module Style exposing (..)

import Html exposing (Attribute)
import Html.Attributes as Html


pageStyle : Attribute msg
pageStyle =
    Html.style
        [ ( "width", "344px" )
        , ( "margin", "20px" )
        , ( "border-style", "solid" )
        , ( "border-width", "2px" )
        , ( "border-color", "darkgray" )
        , ( "border-radius", "5px" )
        , ( "padding", "20px" )
        ]


displayStyle : Attribute msg
displayStyle =
    Html.style
        [ ( "background-color", "#F0F0F0" )
        ]


panelStyle : Attribute msg
panelStyle =
    Html.style
        [ ( "margin-top", "20px" )
        ]
