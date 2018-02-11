module Subscription exposing (..)

import AnimationFrame
import Model exposing (..)


subscriptions : Model -> Sub Msg
subscriptions _ =
    AnimationFrame.diffs NewFrame
