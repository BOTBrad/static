module Static exposing (..)

import Html
import Keyboard

main =
  Html.program
    { init = (0, Cmd.none)
    , subscriptions = \_ -> Keyboard.downs identity
    , view = toString >> Html.text
    , update = \c model -> (c, Cmd.none)
    }
