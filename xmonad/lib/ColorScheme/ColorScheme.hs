module ColorScheme.ColorScheme where

import XMonad

data ColorScheme = ColorScheme { name :: String
                               , bg :: String
                               , bgAlt :: String
                               , fg :: String
                               , primary :: String
                               , secondary :: String
                               , success :: String
                               , danger :: String
                               , warning :: String
                               , info :: String
                               }
