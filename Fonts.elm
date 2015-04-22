module Fonts (big, medium, small, smallBold, iuStyle) where
{-| custom fonts used by our app -}

import Text
import Maybe exposing (Maybe(..))
import Color exposing (white)
import Graphics.Element as E

-- FONTS
big =
    { typeface = [ "BentonSansBold", "sans" ]
    , height   = Just 72
    , color    = white
    , bold     = False
    , italic   = False
    , line     = Nothing
    }

medium =
    { big
    | typeface <- [ "BentonSansRegular", "sans" ]
    , height   <- Just 36
    }

small = { medium | height <- Just 16 }
smallBold = { big | height <- Just 16 }

iuStyle : Text.Style -> String -> E.Element
iuStyle s t = E.centered (Text.style s (Text.fromString t))
