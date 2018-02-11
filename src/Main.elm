module Main exposing (..)

import Html exposing (program)
import Model exposing (..)
import Update exposing (..)
import Subscription exposing (..)
import View exposing (..)


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
