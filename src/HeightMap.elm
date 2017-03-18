module HeightMap exposing
  ( HeightMap
  , new
  , percents
  )

import Random.Pcg as Pcg

type HeightMap
  = HeightMap (List Float)


new : Int -> Pcg.Seed -> HeightMap
new size seed =
  let
    gen = Pcg.list 2 <| Pcg.float 0 1
    (lst, seed1) = Pcg.step gen seed
  in
    HeightMap <| newImpl (size // 2) 0.5 seed1 lst

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

percents : HeightMap -> List Int
percents (HeightMap hm) =
  hm
  |> List.map (\a -> a * 100)
  |> List.map round
