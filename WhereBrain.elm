module WhereBrain where

import Graphics.Element (..)
import Graphics.Collage (..)
import Color (..)
import Signal (..)
import List
import Text
import Window

-- geolocation events imported from JavaScript
type alias RawGeo = { lat:Float,lon:Float,hdg:Float }
port geo : Signal RawGeo
type alias BrainGeo = { distance:Float, direction:Float }
bg : BrainGeo
bg = { distance = -1.0, direction = 361.0 }
png = fittedImage 100 100 "flatbrain_white.png"

-- MATH
-- convert raw geolocation data into "BrainGeo" data, used by our app
bfrGeo : RawGeo -> BrainGeo
bfrGeo g =
    let googEarthRadius = 6378137.0 -- meters used by Google Maps
        brainLat = degrees 39.171989
        brainLon = degrees -86.520674
        lat = degrees g.lat
        lon = degrees g.lon
        dLon = abs (lon - brainLon)
        dist = googEarthRadius * acos(sin(brainLat) * sin(lat) + cos(brainLat) * cos(lat) * cos(dLon))
        dir = atan2(sin(dLon) * cos(brainLat)) (cos(lat)*sin(brainLat) - sin(lat)*cos(brainLat)*cos(dLon))
    in
       { bg | distance <- dist
       , direction <- dir }

-- SCENE
bigFont =
    { typeface = [ "BentonSansBold", "sans" ]
    , height   = Just 72
    , color    = white
    , bold     = False
    , italic   = False
    , line     = Nothing }

medFont =
    { bigFont
    | typeface <- [ "BentonSansRegular", "sans" ]
    , height   <- Just 36 }

iuStyle s t = Text.fromString t |> Text.style s |> Text.centered

-- format meters as string
distMessage : Float -> Element
distMessage d =
    let foot x = x * 3.281 -- meters to feet
        mile x = x * 0.62137 / 1000 -- meters to miles
        inch x = 12 * foot x -- meters to inches
        kilo x = x / 1000
        en = iuStyle bigFont <| toString <| floor n
        ec = width (widthOf en) <| iuStyle medFont c
        (n,c) =
            if | d < 100   -> (inch d, "INCHES")
               | d < 1000  -> (foot d, "FEET")
               | d < 10000 -> (mile d, "MILES")
               | otherwise -> (kilo d, "KILOMETERS")
    in flow down [en, ec]

-- combine background and foreground
scene : (Int, Int) -> RawGeo -> Element
scene (w,h) g =
    let
        eWords    = distMessage (.distance (bfrGeo g))
        words     = toForm eWords
        gDir      = bfrGeo g |> .direction
        wpng      = widthOf png |> toFloat
        wWords    = toFloat <| widthOf eWords

        appLineStyle = {defaultLine | width <- 10, color <- white }
        arrowLine = segment (fromPolar(wWords - 50, gDir)) (fromPolar(wWords + 50, gDir)) |> traced appLineStyle
        circ = outlined appLineStyle (circle wWords)

        -- background = rect (toFloat w) (toFloat h) |> filled red
    in
       color red <| container w h middle <|
           collage w h
           [ png |> toForm |> moveY (toFloat h / 2 - wpng)
           , arrowLine
           , circ
           , words
           ]

-- RENDER
main = scene <~ Window.dimensions ~ geo

