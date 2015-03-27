module Compass (fromRaw,rose,RawGeo,Model) where
import Graphics.Collage as GC
import Graphics.Element as GE
import Color as C
import List

-- MODEL
type alias RawGeo = { lat:Float,lon:Float,hdg:Float }
type alias Model = { angle:Float, dist:Int, units:String }

-- convenience function to convert raw coords to (dist,direction), in units of (meters,radians)
convert : RawGeo -> (Float,Float)
convert rg =
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
       (dist,angl)


-- simplify model to something like (0.23,10,"FEET")
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


circlePlot : Int -> Float -> GE.Element
circlePlot h t =
    let ln = GC.defaultLine
        myLine = { ln | width <- 3, color <- C.white }
        rad = toFloat h / 4
        side = floor rad * 2
        (x1,y1) = fromPolar (rad-20,t)
        (x2,y2) = fromPolar (rad,t)
        pth = GC.segment (x1,y1) (x2,y2)
    in
       GC.collage side side [ GC.traced myLine pth
                            , GC.outlined myLine <| GC.circle (rad-10)
                            ]


logo : Int -> GE.Element
logo h =
    let x = h // 5
        png = GE.fittedImage x x "assets/flatbrain_white.png"
    in GE.container x x GE.middle png


-- VIEW
rose : (Int,Int) -> Model -> GE.Element
rose (w,h) m =
    let ln = GC.defaultLine
        passion = C.rgb 221 30 52
        side = h // 4
        c = circlePlot h m.angle
        widest = GE.widthOf c
    in
       GE.color passion <| GE.container w h GE.middle <|
       GE.flow GE.down <| List.map (\n-> GE.width widest n) [ logo h
                                                            , c
                                                            ]
