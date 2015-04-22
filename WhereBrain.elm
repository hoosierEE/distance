module WhereBrain where

import Color exposing (..)
import Fonts exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List
import Signal exposing (..)
import Text
import Window

-- geolocation events from JavaScript
type alias RawGeo = { lat:Float,lon:Float,hdg:Float }
type alias DistMessage = { dist:Int, msg:String }
port geo : Signal RawGeo

-- a more convenient format
type alias BrainGeo = { distance:Float, direction:Float }
bg : BrainGeo
bg = { distance = -1.0, direction = 0.0 }

-- MATH
-- convert raw geolocation data into "BrainGeo" data, used by our app
bfrGeo : RawGeo -> BrainGeo
bfrGeo g =
    let googEarthRadius = 6378137 -- meters used by Google Maps
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

-- format meters as string
distMessage : Float -> DistMessage
distMessage d =
    let foot x = x * 3.281 -- meters to feet
        mile x = x * 0.62137 / 1000 -- meters to miles
        inch x = 12 * foot x -- meters to inches
        kilo x = x / 1000
    in
        if | d < 100   -> {dist = inch d, msg = "INCHES"}
           | d < 1000  -> {dist = foot d, msg = "FEET"}
           | d < 10000 -> {dist = mile d, msg = "MILES"}
           | otherwise -> {dist = kilo d, msg = "KILOMETERS"}

-- combine background and foreground
scene : (Int, Int) -> RawGeo -> Element
scene (w,h) g =
    let
        eWords = distMessage sixth (.distance <| bfrGeo g)
        gDir   = .direction <| bfrGeo g
        hpng   = toFloat h / 6
        sixth  = h // 6
        fSixth = sixth * 5
        pngOff = (toFloat h - radius) / 2
        (radius,_) = toPolar (toFloat <| widthOf eWords, toFloat <| heightOf eWords)
        img h  = width w <| flow down
                    [ fittedImage h h "assets/flatbrain_white.png"
                    , width h <| iuStyle smFont "WHERE IS"
                    , width h <| iuStyle smBold "#IUBRAIN?"
                    ]
        sLine  = {defaultLine | width <- 3, color <- white }
        lower  = moveY (hpng * -0.75)
        circ h = collage fSixth fSixth
                    [ segment (fromPolar(radius * 0.9, gDir)) (fromPolar(radius * 1.1, gDir)) |> traced sLine |> lower
                    , circle radius |> outlined sLine |> lower
                    , toForm eWords |> lower
                    ]
    in
       color (rgb 221 30 52) <| (flow down << List.map (width w))
            [ img (w//6)
            , circ (fSixth)
            ]

-- RENDER
main = scene <~ Window.dimensions ~ geo

