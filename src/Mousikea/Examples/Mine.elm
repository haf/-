module Mousikea.Examples.Mine exposing (music)

import Mousikea.Music as Music exposing (..)
import Mousikea.Primitive exposing (Dur, Primitive(..))
import Mousikea.Util.Ratio exposing (add, div, divByInt, mul, mulByInt, over)


mainVoice : Music Pitch
mainVoice =
    line
        [ a 4 en
        , cf 4 en
        , cff 4 en
        ]


music : Music1
music =
    let
        things =
            tempo (over 60 120) mainVoice
                |> Music.map (\p -> ( p, [ Volume 60 ] ))
    in
    instrument RhodesPiano things
