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
specializeGeo : RawGeo -> BrainGeo
specializeGeo g =
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
bigFont : Text.Style
bigFont =
    { typeface = [ "BentonSansBold", "sans" ]
    , height   = Just 72
    , color    = white
    , bold     = False
    , italic   = False
    , line     = Nothing
    }

medFont : Text.Style
medFont =
    { bigFont
    | typeface <- [ "BentonSansRegular", "sans" ]
    , height   <- Just 36
    }

iuStyle : Text.Style -> String -> Element
iuStyle s t = Text.fromString t |> Text.style s |> Text.centered
--iuStyle s t = Text.centered <| Text.style s <| Text.fromString t

-- format meters as string
distMessage : Float -> Float -> Element
distMessage s d =
    let foot x = x * 3.281 -- meters to feet
        mile x = x * 0.62137 / 1000 -- meters to miles
        inch x = 12 * foot x -- meters to inches
        kilo x = x / 1000
        (n,c) = if | d < 100   -> (inch d, "INCHES")
                   | d < 1000  -> (foot d, "FEET")
                   | d < 10000 -> (mile d, "MILES")
                   | otherwise -> (kilo d, "KILOMETERS")
        en = iuStyle bigFont <| toString <| floor n
        ec = width (widthOf en) <| iuStyle medFont c
    in flow down [en, ec]

-- combine background and foreground
scene : (Int, Int) -> RawGeo -> Element
scene (w,h) g =
    let tDis = distMessage mrad <| .distance <| specializeGeo g
        tDir = Text.asText <| .direction <| specializeGeo g
        mrad = (toFloat <| List.minimum [w,h]) / 3.0
        wpng = widthOf png |> toFloat
        dis = toForm tDis
        background = rect (toFloat w) (toFloat h) |> filled red
        circ = outlined {defaultLine | width <- 10, color <- white} (circle mrad)
    in
       collage w h <| List.map (\n -> moveY -20.0 n)
       [ background
       , png |> toForm |> moveY (toFloat h / 2 - wpng)
       , circ
       , dis
       ]

-- RENDER
main = scene <~ Window.dimensions ~ geo

