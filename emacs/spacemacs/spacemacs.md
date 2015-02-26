# Spacemacs configuration
This file is loaded by Spacemacs at startup. It must be stored in your home directory.

[:tangle spacemacs.symlink]

## Modelines
```lisp
;; -*- mode: emacs-lisp -*-
;; vim: set ft=lisp:
```

## Configuration Layers 
```lisp
(setq-default
```

List of additional paths where to look for configuration layers.
Paths must have a trailing slash (ie. `~/.mycontribs/')
```lisp
dotspacemacs-configuration-layer-path '("~/.spacemacs_private/")
```
List of configuration layers to load.
```lisp
  dotspacemacs-configuration-layers '(
                                      themes-megapack
                                      vct-layer
                                     )
```
A list of packages and/or extensions that will not be install and loaded.
```lisp
dotspacemacs-excluded-packages '()
```

Finally, we 
```lisp
)
```

# Settings
```lisp
(setq-default
```

Specify the startup banner. If the value is an integer then the
banner with the corresponding index is used, if the value is `random'
then the banner is chosen randomly among the available banners, if
the value is nil then no banner is displayed.
```lisp
dotspacemacs-startup-banner nil
```

Default theme applied at startup
```lisp
dotspacemacs-themes '(monokai)
```

The leader key
```lisp
dotspacemacs-leader-key "SPC"
```

Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`
```lisp
dotspacemacs-major-mode-leader-key ","
```

The command key used for Evil commands (ex-commands) and
Emacs commands (M-x).
By default the command key is `:' so ex-commands are executed like in Vim
with `:` and Emacs commands are executed with `<leader> :`.
```lisp
dotspacemacs-command-key ":"
```

Guide-key delay in seconds. The Guide-key is the popup buffer listing
the commands bound to the current keystrokes.
```lisp
dotspacemacs-guide-key-delay 0.4
```

If non nil the frame is fullscreen when Emacs starts up (Emacs 24.4+ only).
```lisp
dotspacemacs-fullscreen-at-startup nil
```

If non nil `spacemacs/toggle-fullscreen` will not use native fullscreen.
Use to disable fullscreen animations in OSX."
```lisp
dotspacemacs-fullscreen-use-non-native nil
```

If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only).
Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
```lisp
dotspacemacs-maximized-at-startup nil
```

A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency can
be toggled through `toggle-transparency'.
```lisp
dotspacemacs-active-transparency 90
```

A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'.
```lisp
dotspacemacs-inactive-transparency 90
```

If non nil unicode symbols are displayed in the mode line (e.g. for lighters)
```lisp
dotspacemacs-mode-line-unicode-symbols t
```

If non nil smooth scrolling (native-scrolling) is enabled. Smooth scrolling
overrides the default behavior of Emacs which recenters the point when
it reaches the top or bottom of the screen
```lisp
dotspacemacs-smooth-scrolling t
```

If non nil pressing 'jk' in insert state, ido or helm will activate the
evil leader.
```lisp
dotspacemacs-feature-toggle-leader-on-jk nil
```

If non-nil smartparens-strict-mode will be enabled in programming modes.
```lisp
dotspacemacs-smartparens-strict-mode nil
```

If non nil advises quit functions to keep server open when quitting.
```lisp
dotspacemacs-persistent-server nil
```

The default package repository used if no explicit repository has been
specified with an installed package.
Not used for now.
```lisp
dotspacemacs-default-package-repository nil
```

I don't like Adobe pro fonts :P
```lisp
dotspacemacs-default-font '("Inconsolata" :size 14)
```

Finally, we finish the `setq-default` block:
```lisp
)
```

# Initialization Hooks
User initialization for Spacemacs. This function is called at the very startup.
```lisp
(defun dotspacemacs/init ()
)
```

This is were you can ultimately override default Spacemacs configuration.  This function is called at the very end of Spacemacs initialization.
```lisp
(defun dotspacemacs/config ()
)
```
