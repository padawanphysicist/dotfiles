module Bar.Polybar where

import XMonad
import Text.Printf
import ColorScheme.ColorScheme

vctCreatePolybarConfig :: ColorScheme -> IO()
vctCreatePolybarConfig sch = do    
    writeFile "/home/vct/.config/polybar/config.ini" $ "\
\[colors] \n\
\background = "++ bg  sch ++" \n\
\background-alt = #373B41 \n\
\foreground = "++ fg  sch ++" \n\
\primary = "++ primary sch ++" \n\
\secondary = secondary sch\n\
\alert = warning sch \n\
\disabled = bgAlt sch \n\
\ \n\
\[bar/example] \n\
\width = 100% \n\
\height = 24pt \n\
\radius = 4 \n\
\background = ${colors.background} \n\
\foreground = ${colors.foreground} \n\
\ \n\
\line-size = 3pt \n\
\ \n\
\border-size = 4pt \n\
\border-color = #00000000 \n\
\ \n\
\padding-left = 0 \n\
\padding-right = 1 \n\
\ \n\
\module-margin = 1 \n\
\ \n\
\separator = | \n\
\separator-foreground = ${colors.disabled} \n\
\ \n\
\font-0 = \"Noto Sans Mono\" \n\
\cursor-click = pointer \n\
\cursor-scroll = ns-resize \n\
\ \n\
\enable-ipc = true \n\
\ \n\
\; tray-position = right \n\
\modules-left = xworkspaces xwindow \n\
\modules-right = filesystem pulseaudio xkeyboard memory cpu wlan battery date \n\
\ \n\
\[module/xworkspaces] \n\
\type = internal/xworkspaces \n\
\ \n\
\label-active = %name% \n\
\label-active-background = ${colors.background-alt} \n\
\label-active-underline= ${colors.primary} \n\
\label-active-padding = 1 \n\
\ \n\
\label-occupied = %name% \n\
\label-occupied-padding = 1 \n\
\ \n\
\label-urgent = %name% \n\
\label-urgent-background = ${colors.alert} \n\
\label-urgent-padding = 1 \n\
\ \n\
\label-empty = %name% \n\
\label-empty-foreground = ${colors.disabled} \n\
\label-empty-padding = 1 \n\
\ \n\
\[module/xwindow] \n\
\type = internal/xwindow \n\
\label = %title:0:20:...% \n\
\[module/filesystem] \n\
\type = internal/fs \n\
\interval = 25 \n\
\ \n\
\mount-0 = / \n\
\ \n\
\label-mounted = %{F#F0C674}%mountpoint%%{F-} %percentage_used%% \n\
\ \n\
\label-unmounted = %mountpoint% not mounted \n\
\label-unmounted-foreground = ${colors.disabled} \n\
\[module/pulseaudio] \n\
\type = internal/pulseaudio \n\
\ \n\
\format-volume-prefix = \"VOL\" \n\
\format-volume-prefix-foreground = ${colors.primary} \n\
\format-volume = <label-volume> \n\
\ \n\
\label-volume = %percentage%% \n\
\ \n\
\label-muted = muted \n\
\label-muted-foreground = ${colors.disabled} \n\
\[module/xkeyboard] \n\
\type = internal/xkeyboard \n\
\blacklist-0 = num lock \n\
\ \n\
\label-layout = %layout% \n\
\label-layout-foreground = ${colors.primary} \n\
\ \n\
\label-indicator-padding = 2 \n\
\label-indicator-margin = 1 \n\
\label-indicator-foreground = ${colors.background} \n\
\label-indicator-background = ${colors.secondary} \n\
\ \n\
\[module/memory] \n\
\type = internal/memory \n\
\interval = 2 \n\
\format-prefix = \"RAM \" \n\
\format-prefix-foreground = ${colors.primary} \n\
\label = %percentage_used:2%% \n\
\ \n\
\[module/cpu] \n\
\type = internal/cpu \n\
\interval = 2 \n\
\format-prefix = \"CPU \" \n\
\format-prefix-foreground = ${colors.primary} \n\
\label = %percentage:2%% \n\
\ \n\
\[network-base] \n\
\type = internal/network \n\
\interval = 5 \n\
\format-connected = <label-connected> \n\
\format-disconnected = <label-disconnected> \n\
\label-disconnected = %{F#F0C674}%ifname%%{F#707880} disconnected \n\
\ \n\
\[module/wlan] \n\
\inherit = network-base \n\
\interface-type = wireless \n\
\label-connected = %{F#F0C674}%ifname%%{F-} %essid% %local_ip% \n\
\ \n\
\[module/eth] \n\
\inherit = network-base \n\
\interface-type = wired \n\
\label-connected = %{F#F0C674}%ifname%%{F-} %local_ip% \n\
\ \n\
\[module/date] \n\
\type = internal/date \n\
\interval = 1 \n\
\ \n\
\date = %H:%M \n\
\date-alt = %Y-%m-%d %H:%M:%S \n\
\ \n\
\label = %date% \n\
\label-foreground = ${colors.primary} \n\
\ \n\
\[module/battery] \n\
\type = internal/battery \n\
\full-at = 99 \n\
\low-at = 10 \n\
\battery = BAT0 \n\
\adapter = AC \n\
\poll-interval = 5 \n\
\ \n\
\[settings] \n\
\screenchange-reload = true \n\
\pseudo-transparency = true"
