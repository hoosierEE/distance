module Compass (fromRaw,rose,RawGeo,Model) where
import Graphics.Collage as GC
import Graphics.Element as GE
import Color as C

-- MODEL
type alias RawGeo = { lat:Float,lon:Float,hdg:Float }
type alias Model = { angle:Float, dist:Int, units:String }

-- convenience function to convert raw coords to dist/direction
convert : RawGeo -> (Float,Float)
convert rg =
    let googRadius = 6378137.0 -- (somewhat arbitrary) number of meters used by Google Maps
        brainLat = degrees 39.171989
        brainLon = degrees -86.520674
        lat = degrees rg.lat
        lon = degrees rg.lon
        dLon = abs (lon - brainLon)
        dist = googRadius * acos(sin(brainLat) * sin(lat) + cos(brainLat) * cos(lat) * cos(dLon)) -- meters
        arg1 = sin dLon * cos brainLat
        arg2 = cos lat * sin brainLat - sin lat * cos brainLat * cos dLon
        angl = atan2 arg1 arg2
    in
       (dist,angl)


-- simplify floating-point meters to something like (0.23,10,"FEET")
simplify : (Float,Float) -> (Float,Int,String)
simplify (d,o) =
    let foot m = floor <| m * 3.281
        mile m = floor <| m * 0.62137 / 1000
        inch m = foot m * 12
    in
       if | d < 50    -> (o, inch d, "INCHES")
          | d < 1000  -> (o, foot d, "FEET")
          | otherwise -> (o, mile d, "MILES")


fromRaw : RawGeo -> Model
fromRaw g =
    let toModel (f,i,s) = { angle=f, dist=i, units=s }
    in toModel <| simplify <| convert g


-- VIEW
rose : (Int,Int) -> Model -> GE.Element
rose (w,h) m =
    let ln = GC.defaultLine
        passion = C.rgb 221 30 52
        myLine = { ln | width <- 3, color <- C.white }
        rad = toFloat h / 4
        side = floor rad * 2
        (x1,y1) = fromPolar (rad-20,m.angle)
        (x2,y2) = fromPolar (rad,m.angle)
        pth = GC.segment (x1,y1) (x2,y2)
    in
       GE.color passion <| GE.container w h GE.middle <|
       GE.flow GE.down
       [
           GC.collage side side
           [ GC.traced myLine pth
           , GC.outlined myLine <| GC.circle (rad-10)
           ]
       ]
