# Tmux
Configuration for [tmux](http://tmux.sourceforge.net/).

[:tangle tmux.conf.symlink]

## Changing  prefix
The prefix is used to distinguish tmux commands from the commands sent to the programs inside of it. Following GNU Screen prefix, we change the default prefix to `C-a`.

First we release the default prefix,
```sh
unbind C-b  # Release the default prefix
```
and then we define a new prefix:
```sh
set -g prefix C-a
bind-key C-a send-prefix
```

## General options
### Window indexing
Start window indexing at one instead of zero. Then it will be more like the keyboard layout
```sh
set -g base-index 1
setw -g pane-base-index 1
```

Ring the bell if any background window rang a bell
```sh
set -g bell-action any
```

### Weird colors
Fix weird behaviour on vim colors in solarized theme
```sh
set -g default-terminal "screen-256color"
```
### Scrollback size
Use vi keybindings for tmux commandline input.
Note that to get command mode you need to hit ESC twice&#x2026;
```sh
set -g status-keys emacs
```

Use vi keybindings in copy and choice modes
```sh
setw -g mode-keys vi
```

status bar
```sh
set-option -g status-utf8 on
```

Allows for faster key repetition
```sh
set -s escape-time 0
```

Rather than constraining window size to the maximum size of any client connected to the **session**, constrain window size to the maximum size of any client connected to **that window**. Much more reasonable.
```sh
setw -g aggressive-resize on
```

### Pane switching

For pane switching it will be used the combination `Alt+<arrow key>`
```sh
bind -n M-Left select-pane -L
bind -n M-Right select-pane -R
bind -n M-Up select-pane -U
bind -n M-Down select-pane -D
```

### Activity monitoring

In the case of multiple opened windows, you will get notified when something happens inside the other windows. The commands below will cause tmux to write a message and highlight the window inside of which the activity took place
```sh
setw -g monitor-activity on
set -g visual-activity on
```

### auto window rename
```sh
set-window-option -g automatic-rename
```

### use utf8
```sh
set -g utf8
set-window-option -g utf8 on
```

## Pane management / navigation

### Horizontal splits with `s` or `^S`
```sh
unbind s
unbind ^S
bind-key s split-window
bind-key ^S split-window
```

### Vertical split with `v` or `^V`
```sh
unbind v
unbind ^V
bind-key v split-window -h
bind-key ^V split-window -h
```

## Mouse mode
```sh
set -g mode-mouse on
set -g mouse-resize-pane on
set -g mouse-select-pane on
set -g mouse-select-window on
```

### Toggle mouse on
```sh
bind m \
    set -g mode-mouse on \;\
    set -g mouse-resize-pane on \;\
    set -g mouse-select-pane on \;\
    set -g mouse-select-window on \;\
    display 'Mouse: ON'

bind M \
    set -g mode-mouse off \;\
    set -g mouse-resize-pane off \;\
    set -g mouse-select-pane off \;\
    set -g mouse-select-window off \;\
    display 'Mouse: OFF'
```
## Colors

extracted from [here](<https://github.com/seebi/tmux-colors-solarized/blob/master/tmuxcolors-256.conf>)

Default statusbar colors
```sh
set-option -g status-bg colour235 #base02
set-option -g status-fg colour136 #yellow
set-option -g status-attr default
```

Default window title colors
```sh
set-window-option -g window-status-fg colour244 #base0
set-window-option -g window-status-bg default
```
Active window title colors
```sh
set-window-option -g window-status-current-fg colour166 #orange
set-window-option -g window-status-current-bg default
```
Pane border
```sh
set-option -g pane-border-fg colour235 #base02
set-option -g pane-active-border-fg colour240 #base01
```

Message text
```sh
set-option -g message-bg colour235 #base02
set-option -g message-fg colour166 #orange
```

Pane number display
```sh
set-option -g display-panes-active-colour colour33 #blue
set-option -g display-panes-colour colour166 #orange
```

Clock
```sh
set-window-option -g clock-mode-colour green #green
```

## Misc settings

extracted from [here](<https://github.com/tony/tmux-config>)
```sh
set -g status-interval 1
set -g status-justify centre # center align window list
set -g status-left-length 20
set -g status-right-length 140
set -g status-left '#[fg=green]#H #[fg=black]• #[fg=green,bright]#(uname -r | cut -c 1-6)#[default]'
set -g status-right '#[fg=green,bg=default,bright]#(tmux-mem-cpu-load 1) #[fg=red,dim,bg=default]#(uptime | cut -f 4-5 -d " " | cut -f 1 -d ",") #[fg=white,bg=default]%a%l:%M:%S %p#[default] #[fg=blue]%Y-%m-%d'
```
Solves slight delay when switching modes in vim
```sh
set -sg escape-time 0
```