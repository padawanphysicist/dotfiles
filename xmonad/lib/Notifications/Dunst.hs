module Notifications.Dunst where

import XMonad
import Text.Printf
import ColorScheme.ColorScheme

vctCreateDunstConfig :: ColorScheme -> IO()
vctCreateDunstConfig sch = do    
    writeFile "/home/vct/.config/dunst/dunstrc" $ "\
\[urgency_normal] \n\
\    background = \""++ bg sch ++"\" \n\
\    foreground = \""++ fg sch ++"\" \n\
\    timeout = 10 \n\
\[global] \n\
\    font = \"SF Pro Display Regular 14\" \n\
\    transparency = 30 \n\
\    alignment = center \n\
\    word_wrap = yes \n\
\    frame_width = 0 \n\
\    markup = full \n\
\    format = \"%s %p\n%b\" \n\
\    show_age_threshold = 60 \n\
\    idle_threshold = 120 \n\
\    sticky_history = yes \n\
\    corner_radius = 6 \n\
\    icon_position = left \n\
\    icon_theme = breeze \n\
\    enable_recursive_icon_lookup = true"
