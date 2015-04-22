module Main where
{-| Wire up the whole application -}

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Compass exposing (RawGeo, BrainGeo, fromRaw)
import Fonts exposing (bigFont, medFont, smFont, smBold, iuStyle)
import Signal exposing ((<~),(~))
import Color
import Window

type alias DistMessage = { dist: Float, msg: String }

-- format meters as string
distMessage : BrainGeo -> DistMessage
distMessage geo =
    let
        d = .distance geo
        foot x = x * 3.281 -- meters to feet
        mile x = x * 0.62137 / 1000 -- meters to miles
        inch x = 12 * foot x -- meters to inches
        kilo x = x / 1000
    in
        if | d < 100   -> {dist = inch d, msg = "INCHES"}
           | d < 1000  -> {dist = foot d, msg = "FEET"}
           | d < 10000 -> {dist = mile d, msg = "MILES"}
           | otherwise -> {dist = kilo d, msg = "KILOMETERS"}

brainBlock (w,h) =
    let
        h' = h - heightOf cap1 - heightOf cap2
        cap1 = width w <| iuStyle smFont "WHERE IS"
        cap2 = width w <| iuStyle smBold "#IUBRAIN?"
        pic = container w h' middle <| fittedImage h' h' "assets/flatbrain_white.png"
        group = flow down <| List.map (width w) [pic, cap1, cap2]
    in
       container w h middle group

compassBlock (w,h) g =
    let
        r = 0.4 * toFloat (if w < h then w else h)
        bg = fromRaw g
        dm = distMessage bg
        cap1 = width w <| iuStyle bigFont (toString <| floor dm.dist)
        cap2 = width w <| iuStyle medFont dm.msg
        caps = flow down [cap1,cap2]
        sLine  = {defaultLine | width <- 3, color <- Color.white }
        cir = circle r |> outlined sLine
        lin = segment (fromPolar (r * 0.9, bg.direction)) (fromPolar (r * 1.1, bg.direction)) |> traced sLine
    in
       container w h middle (collage w h [cir,lin,toForm caps])

scene (w,h) g =
    let
        ht x = round <| toFloat h * x
        upper = 0.2
        h1 = ht upper
        h2 = ht (1-upper)
    in flow down
        [ brainBlock (w,h1)
        , compassBlock (w,h2) g
        ] |> width w |> color (Color.rgb 221 30 52)

port geo : Signal RawGeo
main = scene <~ Window.dimensions ~ geo

