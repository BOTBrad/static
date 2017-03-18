module HeightMap exposing
  ( HeightMap
  , get
  , new
  , percents
  )

import List.Extra as List
import Random.Pcg as Pcg

type HeightMap
  = HeightMap (List Float)

get : Float -> HeightMap -> Float
get ptRaw (HeightMap hm) =
  let
    pt = max 0 <| min 1 ptRaw
    len = List.length hm
    ptLen = pt * (toFloat len)
    index = floor ptLen
    w0 = ptLen - (toFloat index)
    w1 = 1 - w0
    left = List.getAt index hm |> Maybe.withDefault 0
    right = List.getAt (index + 1) hm |> Maybe.withDefault 0
  in
    left * w0 + right * w1


new : Int -> Pcg.Seed -> HeightMap
new size seed =
  let
    gen = Pcg.list 2 <| Pcg.float 0 1
    (lst, seed1) = Pcg.step gen seed
  in
    HeightMap <| newImpl (size // 2) 0.5 seed1 lst

percents : HeightMap -> List Int
percents (HeightMap hm) =
  hm
  |> List.map (\a -> a * 100)
  |> List.map round

-- new helpers

newImpl : Int -> Float -> Pcg.Seed -> List Float -> List Float
newImpl remaining displacement seed lst =
  if remaining == 0 then
    lst
  else
    let
      (newLst, newSeed) = intersperse displacement seed lst
    in
      newImpl (remaining // 2) (displacement / 2) newSeed newLst

intersperse : Float -> Pcg.Seed -> List Float -> (List Float, Pcg.Seed)
intersperse dis seed lst =
  case lst of
    [] ->
      ([], seed)

    a::[] -> -- this shouldn't happen :\
      (a::[], seed)

    a::c::rst ->
      let
        (ran, newSeed) = Pcg.step (Pcg.float 0 1) seed
        b = (a + c) / 2 + ran * dis
        (rest, finalSeed) = (intersperse (dis / 2) newSeed rst)
      in
        (a::b::c::rest, finalSeed)

