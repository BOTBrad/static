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
    hm = HeightMap.new 480 seed1
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
    num =
      model.key
      |> toString
      |> Text.fromString
      |> Text.height 24
      |> Text.color Color.green
      |> Collage.text
    rows =
      List.range 0 (height - 1)
      |> List.map (\v ->
        v
        |> toFloat
        |> (\v -> (v - height/2, v / (height - 1)))
        |> Tuple.mapSecond (\t ->
          t
          |> (flip HeightMap.get model.hm)
          |> Color.greyscale
        )
        |> (\(off, col) ->
          Collage.rect width 1
          |> Collage.filled col
          |> Collage.moveY off
        )
      )
  in
   [rows, [num]]
   |> List.concat
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
