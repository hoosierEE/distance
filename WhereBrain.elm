module WhereBrain where
import Text
import Compass
import Graphics.Element (..)
import Signal (Signal,(<~))
-- geolocation events from JavaScript
port geo : Signal Compass.RawGeo

-- scene g = Text.asText <| Compass.update g
scene g =
    let cc = Compass.convert g
        gg = Compass.update g
        vals =
            [ Text.plainText "raw: "
            , Text.asText g
            , Text.plainText "convert: "
            , Text.asText cc
            , Text.plainText "update: "
            , Text.asText gg
            ]
    in flow down vals

main = scene <~ geo

