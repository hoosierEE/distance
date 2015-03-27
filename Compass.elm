module Compass (fromRaw,rose,RawGeo,Model) where
import Graphics.Collage as GC
import Graphics.Element (..)
import Color as C
import List
import Text

-- MODEL
type alias RawGeo = { lat:Float,lon:Float }
type alias Model = { angle:Float, dist:Int, units:String }

-- convenience function to convert raw coords to (dist,direction). units: (meters,radians)
fromRaw : RawGeo -> Model
fromRaw rg =
    let googRadius = 6378137.0 -- Earth radius used by Google Maps
        brainLat = degrees 39.171989
        brainLon = degrees -86.520674
        lat = degrees rg.lat
        lon = degrees rg.lon
        dLon = abs (lon - brainLon)
        dist = googRadius * acos(sin(brainLat) * sin(lat) + cos(brainLat) * cos(lat) * cos(dLon))
        arg1 = sin dLon * cos brainLat
        arg2 = cos lat * sin brainLat - sin lat * cos brainLat * cos dLon
        angl = atan2 arg1 arg2
    in
       modelize (dist,angl)


-- modelize model to something like (0.23,10,"FEET")
modelize : (Float,Float) -> Model
modelize (d,o) =
    let foot m = floor <| m * 3.281
        mile m = floor <| m * 0.62137 / 1000
        inch m = foot m * 12
    in
       if | d < 50    -> { angle=o, dist=inch d, units="INCHES" }
          | d < 1000  -> { angle=o, dist=foot d, units="FEET"   }
          | otherwise -> { angle=o, dist=mile d, units="MILES"  }


-- VIEW
circlePlot : Int -> Float -> Element
circlePlot h t =
    let ln = GC.defaultLine
        myLine = { ln | width <- 3, color <- C.white }
        rad = toFloat h / 5
        --side = h // 2
        (x1,y1) = fromPolar (rad-20,t)
        (x2,y2) = fromPolar (rad,t)
        pth = GC.segment (x1,y1) (x2,y2)
    in
       GC.collage h h [ GC.traced myLine pth
                      , GC.outlined myLine <| GC.circle (rad-10)
                      ]


logo : Int -> Element
logo h =
    let x = h // 5
        png = fittedImage x x "assets/flatbrain_white.png"
        cap1 = Text.plainText "WHERE IS"
    in container x x midTop <| flow down [png,cap1]


rose : (Int,Int) -> RawGeo -> Element
rose (w,h) rg =
    let ln = GC.defaultLine
        passion = C.rgb 221 30 52
        m = fromRaw rg
        l = logo h
        c = circlePlot h m.angle
        widest = widthOf c
    in
       color passion <| container w h middle <|
       flow down <| List.map (\n-> width widest n) [ l
                                                   , c
                                                   ]
