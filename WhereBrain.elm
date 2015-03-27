module WhereBrain where
import Compass
import Signal (Signal,(<~),(~))
import Window


-- geolocation events from JavaScript
port geo : Signal Compass.RawGeo

main = Compass.rose <~ Window.dimensions ~ (Compass.fromRaw <~ geo)

