module WhereBrain where

import Signal (..)
import Text

-- Import geolocation events from JS through a port
port geo : Signal {lat:Float,lon:Float,hdg:Float}

-- TODO: calculate great circle distance, bearing.

main = Text.asText <~ geo
