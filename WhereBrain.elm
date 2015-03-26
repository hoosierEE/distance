module WhereBrain where

import Graphics.Element as E
import Graphics.Collage (..)
import Color
import Signal (..)
import List
import Text
import Window

-- geolocation events from JavaScript
type alias RawGeo = { lat:Float,lon:Float,hdg:Float }
port geo : Signal RawGeo
-- a more convenient format
type alias BrainGeo = { distance:Float, direction:Float }
bg : BrainGeo
bg = { distance = -1.0, direction = 361.0 }

-- MATH
-- convert raw geolocation data into "BrainGeo" data, used by our app
update : RawGeo -> BrainGeo
update g =
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

-- FONTS
bigFont =
    { typeface = [ "BentonSansBold", "sans" ]
    , height   = Just 72
    , color    = Color.white
    , bold     = False
    , italic   = False
    , line     = Nothing }

medFont =
    { bigFont
    | typeface <- [ "BentonSansRegular", "sans" ]
    , height   <- Just 36 }

smFont = { medFont | height <- Just 16 }
smFont' = { smFont | typeface <- ["BentonSansBold", "sans"] }

iuStyle s t = Text.fromString t |> Text.style s |> Text.centered

-- format meters as string
distMessage : Float -> E.Element
distMessage d =
    let foot x = x * 3.281 -- meters to feet
        mile x = x * 0.62137 / 1000 -- meters to miles
        inch x = 12 * foot x -- meters to inches
        kilo x = x / 1000
        en = iuStyle bigFont <| toString <| floor n
        ec = iuStyle medFont c
        wTot = List.maximum <| List.map E.widthOf [en,ec]
        elms = List.map (E.width wTot) [en,ec]
        (n,c) =
            if | d < 100   -> (inch d, "INCHES")
               | d < 1000  -> (foot d, "FEET")
               | d < 10000 -> (mile d, "MILES")
               | otherwise -> (kilo d, "KILOMETERS")
    in E.flow E.down elms

-- combine background and foreground
scene : (Int, Int) -> RawGeo -> E.Element
scene (w,h) g =
    let
        eWords = distMessage <| .distance <| update g
        gDir   = .direction <| update g
        hpng   = toFloat h / 6
        hpng'  = floor hpng
        pngOff = (toFloat h - fitRad) / 2
        fitRad = sqrt <| List.sum <| List.map (\n -> toFloat <| n*n) [E.widthOf eWords, E.heightOf eWords]
        thik   = 3
        png    = E.flow E.down
                    [ E.fittedImage hpng' hpng' "assets/flatbrain_white.png"
                    , E.width hpng' <| iuStyle smFont "WHERE IS"
                    , E.width hpng' <| iuStyle smFont' "#IUBRAIN?"
                    ]
        sLine  = {defaultLine | width <- thik, color <- Color.white }
        lower  = moveY (negate hpng * 0.75)
    in
        collage w h
            [ png |> toForm |> moveY pngOff
            , segment (fromPolar(fitRad * 0.9, gDir)) (fromPolar(fitRad * 1.1, gDir)) |> traced sLine |> lower
            , circle fitRad |> outlined sLine |> lower
            , toForm eWords |> lower
            ] |> E.container w h E.middle |> E.color (Color.rgb 221 30 52)

-- RENDER
main = scene <~ Window.dimensions ~ geo

