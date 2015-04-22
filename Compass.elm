module Compass (BrainGeo, RawGeo, bfrGeo) where
{-| Convert raw geolocation data into something our app can use -}

type alias BrainGeo    = { distance:Float, direction:Float }
type alias DistMessage = { dist:Int, msg:String }
type alias RawGeo      = { lat:Float,lon:Float,hdg:Float }

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

