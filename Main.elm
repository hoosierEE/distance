module Main where

------------------------------
-- STANDARD LIBRARY IMPORTS --
------------------------------
import Color
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Signal exposing ((<~),(~))
import Window

-------------------
-- LOCAL IMPORTS --
-------------------
import Compass
import Fonts

------------
-- RENDER --
------------
brainBlock : (Int,Int) -> Element
brainBlock (w,h) =
    let
        cap1 = width w <| Fonts.iuStyle Fonts.small "WHERE IS"
        cap2 = width w <| Fonts.iuStyle Fonts.smallBold "#IUBRAIN?"
        hh = h - heightOf cap1 - heightOf cap2
        pic = container w hh middle <| fittedImage hh hh "assets/flatbrain_white.png"
        group = flow down [pic, cap1, cap2]
    in
       container w h middle group |> link "https://twitter.com/iubrain"

compassBlock : (Int,Int) -> Compass.RawGeo -> Element
compassBlock (w,h) g =
    let
        r = 0.4 * toFloat (if w < h then w else h)
        bg = Compass.fromRaw g
        dm = distMessage bg
        bgd = pi/2 + bg.direction
        cap1 = width w <| Fonts.iuStyle Fonts.big (toString <| floor dm.dist)
        cap2 = width w <| Fonts.iuStyle Fonts.medium dm.msg
        caps = flow down [cap1,cap2]
        sLine  = {defaultLine | width <- 3, color <- Color.white }
        cir = circle r |> outlined sLine
        lin = segment (fromPolar (r * 0.9, bgd)) (fromPolar (r * 1.1, bgd)) |> traced sLine
    in
       container w h middle (collage w h [cir,lin,toForm caps]) |> link "http://psych.indiana.edu/"

scene : (Int,Int) -> Compass.RawGeo -> Element
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


-----------
-- PORTS --
-----------
port geo : Signal Compass.RawGeo


----------
-- MAIN --
----------
main : Signal Element
main = scene <~ Window.dimensions ~ geo


-------------
-- HELPERS --
-------------
type alias DistMessage = { dist: Float, msg: String }
-- format meters as string
distMessage : Compass.BrainGeo -> DistMessage
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

