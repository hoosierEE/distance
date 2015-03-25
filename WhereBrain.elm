module WhereBrain where

import Graphics.Element (Element, container, middle, flow, down)
import Graphics.Collage (..)
import Color (..)
import Signal (..)
import List (minimum)
import Text
import Window
-- geolocation events imported from JavaScript
type alias RawGeo = { lat:Float,lon:Float,hdg:Float }
port geo : Signal RawGeo
type alias BrainGeo = { distance:Float, direction:Float }
bg : BrainGeo
bg = { distance = -1.0, direction = 361.0 }

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
    { typeface = [ "Belton Sans", "sans" ]
    , height   = Just 36
    , color    = white
    , bold     = False
    , italic   = False
    , line     = Nothing
    }

-- style some text with bigFont
bigStyle : String -> Element
bigStyle t =
    let tt = Text.fromString t
        st = Text.style bigFont tt
    in Text.centered st

-- format meters as string
distMessage : Float -> Element
distMessage d =
    let foot x = x * 3.281 -- meters to feet
        mile x = x * 0.62137 / 1000 -- meters to miles
        inch x = 12 * foot x -- meters to inches
        pstr = toString << floor -- round down, convert to String
    in
       if | d < 100 -> bigStyle <| pstr (inch d) ++ " inches"
          | d < 1000 -> bigStyle <| pstr (foot d) ++ " feet"
          | d < 10000 -> bigStyle <| pstr (mile d) ++ " miles"
          | otherwise -> bigStyle <| pstr (d / 1000) ++ " km"

-- combine background and foreground
scene : (Int, Int) -> RawGeo -> Element
scene (w,h) g =
    let tDis = distMessage <| .distance <| specializeGeo g
        tDir = Text.asText <| .direction <| specializeGeo g
        mrad = toFloat <| (minimum [w,h]) // 3
        -- forms
        dis = toForm tDis
        background = rect (toFloat w) (toFloat h) |> filled red
        circ = outlined (solid white) (circle mrad)
    in collage w h
       [ background
       , circ
       , dis
       ]

-- RENDER
main = scene <~ Window.dimensions ~ geo

