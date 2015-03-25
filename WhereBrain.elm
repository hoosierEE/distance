module WhereBrain where
import Graphics.Element (..)
import Signal (..)
import Text
import Window
-- geolocation events imported from JavaScript
port geo : Signal {lat:Float,lon:Float,hdg:Float}
type alias BrainGeo = { distance:Float, direction:Float }
bg : BrainGeo
bg = { distance = -1.0, direction = 361.0 }

-- MATHS
--------
geodesy g = -- convert raw latitude, longitude (, and heading?) to distance and direction
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

-- distance to "convenient" string
distString : Float -> String
distString d = -- convert a distance in meters to more convenient units
    if | d < 100 -> toString d ++ " meters"
       | d < 1000 -> toString (d / 1000) ++ " kilometers"
       | otherwise -> "really far"

-- SCENE
--------
scene (w,h) g =
    let tDir = Text.plainText (distString ((geodesy g).distance))
        tDis = Text.asText (geodesy g).direction
        boths = flow down [tDir, tDis]
    in container w h middle boths

-- RENDER
---------
main = scene <~ Window.dimensions ~ geo

