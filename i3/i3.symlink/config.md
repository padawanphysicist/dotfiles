# i3 configuration

[:tangle config]

## Change Keyboard Layout
```
exec setxkbmap -model abnt2 -layout br -variant abnt2
```

## Change mod key
```
set $mod Mod4
```

## Setting colors
### Window Titles
This font will also be used by the bar unless a different font is used in the bar `{}` block below. This font is widely installed, provides lots of unicode glyphs, right-to-left text rendering and scalability on retina/hidpi displays (thanks to pango).
```
font pango:Droid Sans Mono 8
```

## Use Mouse+$mod to drag floating windows to their wanted position
```
floating_modifier $mod
```

## start a terminal
```
bindsym $mod+Return exec i3-sensible-terminal
```

## kill focused window
```
bindsym $mod+Shift+q kill
```

## start dmenu (a program launcher)
```
bindsym $mod+d exec dmenu_run -fn 'Droid Sans Mono-9'
```

There also is the (new) i3-dmenu-desktop which only displays applications
shipping a .desktop file. It is a wrapper around dmenu, so you need that
installed.

bindsym $mod+d exec --no-startup-id i3-dmenu-desktop

## change focus
```
bindsym $mod+j focus left
bindsym $mod+k focus down
bindsym $mod+l focus up
bindsym $mod+semicolon focus right
```

alternatively, you can use the cursor keys:
```
bindsym $mod+Left focus left
bindsym $mod+Down focus down
bindsym $mod+Up focus up
bindsym $mod+Right focus right
```

## move focused window
```
bindsym $mod+Shift+j move left
bindsym $mod+Shift+k move down
bindsym $mod+Shift+l move up
bindsym $mod+Shift+semicolon move right
```

alternatively, you can use the cursor keys:
```
bindsym $mod+Shift+Left move left
bindsym $mod+Shift+Down move down
bindsym $mod+Shift+Up move up
bindsym $mod+Shift+Right move right
```

## split in horizontal orientation
```
bindsym $mod+h split h
```

## split in vertical orientation
```
bindsym $mod+v split v
```

## enter fullscreen mode for the focused container
```
bindsym $mod+f fullscreen
```

## change container layout (stacked, tabbed, toggle split)
```
bindsym $mod+s layout stacking
bindsym $mod+w layout tabbed
bindsym $mod+e layout toggle split
```

## toggle tiling / floating
```
bindsym $mod+Shift+space floating toggle
```

## change focus between tiling / floating windows
```
bindsym $mod+space focus mode_toggle
```

## focus the parent container
```
bindsym $mod+a focus parent
```

## focus the child container
```
#bindsym $mod+d focus child
```

## switch to workspace
```
bindsym $mod+1 workspace 1
bindsym $mod+2 workspace 2
bindsym $mod+3 workspace 3
bindsym $mod+4 workspace 4
bindsym $mod+5 workspace 5
bindsym $mod+6 workspace 6
bindsym $mod+7 workspace 7
bindsym $mod+8 workspace 8
bindsym $mod+9 workspace 9
bindsym $mod+0 workspace 10
```

## move focused container to workspace
```
bindsym $mod+Shift+1 move container to workspace 1
bindsym $mod+Shift+2 move container to workspace 2
bindsym $mod+Shift+3 move container to workspace 3
bindsym $mod+Shift+4 move container to workspace 4
bindsym $mod+Shift+5 move container to workspace 5
bindsym $mod+Shift+6 move container to workspace 6
bindsym $mod+Shift+7 move container to workspace 7
bindsym $mod+Shift+8 move container to workspace 8
bindsym $mod+Shift+9 move container to workspace 9
bindsym $mod+Shift+0 move container to workspace 10
```

## reload the configuration file
```
bindsym $mod+Shift+c reload
```

## restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
```
bindsym $mod+Shift+r restart
```

## exit i3 (logs you out of your X session)
```
bindsym $mod+Shift+e exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'"
```

## Volume buttons
The volume buttons on the keyboard can be used to adjust the volume by finding the keycode from xev and using that to call a script and adjust the volume.

Since I am using i3, this is fairly simple to do:
```
bindcode 121 exec amixer sset Master toggle
bindcode 122 exec amixer sset Master 2dB-
bindcode 123 exec amixer sset Master 2dB+a
```

## Display Backlight
To begin with I found no way to modify the screen’s brightness, I am unsure exactly what I did to allow this but I believe it was fixed when I installed the samsung-tools package. In any case, if you have that package you should see three files in `/sys/class/backlight`: `acpi_video0`, `acpi_video1` and `intel_backlight`. The backlight can then be adjusted with

    xbacklight -inc 10
    xbacklight -dec 10

This can again be tied to the function keys by adding the following to your i3 configuration:
```
#Laptop Brightness Controls
bindcode 232 exec xbacklight -dec 10
bindcode 233 exec xbacklight -inc 10
```

## resize window (you can also use the mouse for that)
```
mode "resize" {
        # These bindings trigger as soon as you enter the resize mode

        # Pressing left will shrink the window’s width.
        # Pressing right will grow the window’s width.
        # Pressing up will shrink the window’s height.
        # Pressing down will grow the window’s height.
        bindsym j resize shrink width 10 px or 10 ppt
        bindsym k resize grow height 10 px or 10 ppt
        bindsym l resize shrink height 10 px or 10 ppt
        bindsym semicolon resize grow width 10 px or 10 ppt

        # same bindings, but for the arrow keys
        bindsym Left resize shrink width 10 px or 10 ppt
        bindsym Down resize grow height 10 px or 10 ppt
        bindsym Up resize shrink height 10 px or 10 ppt
        bindsym Right resize grow width 10 px or 10 ppt

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
}
bindsym $mod+r mode "resize"
```

## Multiple Monitors
For solving the multiple monitor problems, I use `xrandr`
```
bindsym $mod+m exec xrandr --output HDMI1 --off
bindsym $mod+shift+m exec xrandr --output HDMI1 --auto --right-of LVDS1
```

## panel bar
Start i3bar to display a workspace bar (plus the system information i3status
finds out, if available)
```
bar {
  position top
    status_command i3status
    workspace_buttons yes
  colors {
    separator #444444
    background #222222
    statusline #666666
    focused_workspace #d70a53 #d70a53 #ffffff
    active_workspace #333333 #333333 #888888
    inactive_workspace #333333 #333333 #888888
    urgent_workspace #eb709b #eb709b #ffffff
  }
}
client.focused #d70a53 #d70a53 #ffffff #8c0333
client.focused_inactive #333333 #333333 #888888 #333333
client.unfocused #333333 #333333 #888888 #333333
client.urgent #eb709b #eb709b #ffffff #eb709b
```
