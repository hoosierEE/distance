module Fonts (bigFont, medFont, smFont, smBold, iuStyle) where
{-| custom fonts used by our app -}

import Text
import Maybe exposing (Maybe(..))
import Color exposing (white)
import Graphics.Element as E

-- FONTS
bigFont =
    { typeface = [ "BentonSansBold", "sans" ]
    , height   = Just 72
    , color    = white
    , bold     = False
    , italic   = False
    , line     = Nothing
    }

medFont =
    { bigFont
    | typeface <- [ "BentonSansRegular", "sans" ]
    , height   <- Just 36
    }

smFont = { medFont | height <- Just 16 }
smBold = { bigFont | height <- Just 16 }

iuStyle : Text.Style -> String -> E.Element
iuStyle s t = E.centered (Text.style s (Text.fromString t))
