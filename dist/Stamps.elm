module Stamps where

import Signal (..)
import Text

-- Import geo events from JS
port geo : Signal {lat:Float,lon:Float,hdg:Float}

main = Text.asText <~ geo
