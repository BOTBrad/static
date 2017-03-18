module Static exposing (..)

import Collage
import Color
import Element
import Html
import Keyboard
import Random.Pcg as Pcg
import Text

import HeightMap

main =
  Html.program
    { init = init
    , subscriptions = \_ -> Keyboard.downs identity
    , view = view
    , update = update
    }

type alias Model = 
  { key : Keyboard.KeyCode
  , hm : HeightMap.HeightMap
  , seed : Pcg.Seed
  }

init : (Model, Cmd a)
init =
  let
    seed0 = Pcg.initialSeed 12345
    (seed1, seed) = Pcg.step Pcg.independentSeed seed0
    hm = HeightMap.new 4 seed1
  in
    ( Model
        0
        hm
        seed
    , Cmd.none
    )

view : Model -> Html.Html a
view model =
  let
    (width, height) = (640, 480)
    bg =
      Collage.rect width height
      |> Collage.filled Color.black
    num =
      { key = model.key
      , hm = HeightMap.percents model.hm
      , at = round <| HeightMap.get 0.125 model.hm * 100
      }
      |> toString
      |> Text.fromString
      |> Text.height 24
      |> Text.color Color.white
      |> Collage.text
  in
   [bg, num]
      |> Collage.collage width height
      |> Element.toHtml

update : Keyboard.KeyCode -> Model -> (Model, Cmd a)
update key model =
  let
    m =
      { model
      | key = key
      }
  in
    (m, Cmd.none)
