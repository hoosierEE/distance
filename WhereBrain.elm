module WhereBrain where
import Compass
import Signal (Signal,(<~),(~))
import Window


-- geolocation events from JavaScript
port geo : Signal Compass.RawGeo


-- scene : (Int,Int) -> Compass.RawGeo -> Compass.
main = Compass.rose <~ Window.dimensions ~ geo

