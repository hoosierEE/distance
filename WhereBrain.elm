module WhereBrain where
import Text
import Compass
import Graphics.Element (..)
import Signal (Signal,(<~))
-- geolocation events from JavaScript
port geo : Signal Compass.RawGeo

-- scene g = Text.asText <| Compass.update g
scene g =
    let vals = [ Text.asText <| Compass.convert g
               , Text.asText <| Compass.update g
               , Text.asText g
               ]
    in flow down vals

main = scene <~ geo

