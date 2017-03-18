module Static exposing (..)

import Collage
import Color
import Element
import Html
import Keyboard
import Text

main =
  Html.program
    { init = (0, Cmd.none)
    , subscriptions = \_ -> Keyboard.downs identity
    , view = view
    , update = \c model -> (c, Cmd.none)
    }

type alias Model = Keyboard.KeyCode

view : Model -> Html.Html a
view model =
  let
    (width, height) = (640, 480)
    bg =
      Collage.rect width height
      |> Collage.filled Color.black
    num =
      model
      |> toString
      |> Text.fromString
      |> Text.height 48
      |> Text.color Color.white
      |> Collage.text
  in
   [bg, num]
      |> Collage.collage width height
      |> Element.toHtml

