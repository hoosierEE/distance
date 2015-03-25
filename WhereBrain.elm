module WhereBrain where

import Graphics.Element (..)
import Signal (..)
import Text
import Window

-- import geolocation events from JavaScript
port geo : Signal {lat:Float,lon:Float,hdg:Float}

type alias BrainGeo = { distance:Float, direction:Float }

-- MATHS
dist g = -- convert latitude and longitude to radians
    let googEarthRadius = 6378137.0 -- meters
        brainLat = degrees 39.171989
        brainLon = degrees -86.520674
        lat      = degrees g.lat
        lon      = degrees g.lon
        dLon     = abs (lon - brainLon)
    in
       googEarthRadius * acos(sin(brainLat) * sin(lat) + cos(brainLat) * cos(lat) * cos(dLon))

scene (w,h) g =
    let dtext = Text.asText (dist g)
    in container w h middle dtext

main = scene <~ Window.dimensions ~ geo

