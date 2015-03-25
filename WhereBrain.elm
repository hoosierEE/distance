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
    if | d < 100 -> toString (floor d) ++ " meters"
       | d > 1000 -> toString (floor (0.62137 * d / 1000)) ++ " miles"
       | otherwise -> toString (floor (d /1000)) ++ " km"

-- SCENE
--------
scene : (Int, Int) -> { a | lat : Float, lon : Float } -> Element
scene (w,h) g =
    let tDir = Text.plainText <| distString <| .distance <| geodesy g
        tDis = Text.asText <| .direction <| geodesy g
        boths = flow down [tDir, tDis]
    in container w h middle boths

-- RENDER
---------
main = scene <~ Window.dimensions ~ geo

