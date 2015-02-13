# Configuring Emacs using literate programming
This is another attempt at creating a readable and maintainable Emacs configuration. I'm hopeful that using a literate programming style will help tame the spaghettification.

# Start up
Start with the scratch buffer; no start up screen.
```lisp
(setq inhibit-startup-message t)
```
# General configuration
Here I store configuration common to all files.

## CUA mode
```lisp
    (cua-mode t)
```
## Backup files
Store all backup and autosave files in the tmp dir
```lisp
(setq backup-directory-alist
  `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
  `((".*" ,temporary-file-directory t)))
```
## History
```lisp
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
  '(kill-ring
     search-ring
     regexp-search-ring))
```
## Save cursor position across sessions
Next time you open a file, the cursor will be at the position you last opened it.
```lisp
(require 'saveplace)
(setq-default save-place t)
```

## Change "yes or no" to "y or n"
Thanks to [Sacha Chua](http://pages.sachachua.com/.emacs.d/Sacha.html#unnumbered-16) for this =)
```lisp
(fset 'yes-or-no-p 'y-or-n-p)
```

# Package Management
For installing packages using ELPA, MELPA, Marmalade.
```lisp
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)
)
```
# Path
Add custom paths to emacs.
```lisp
(add-to-list 'load-path "~/.emacs.d/custom/") ; Load custom libraries
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes") ; extra theme path
```
# Graphics and looks
## Color theme
ujelly theme is awesome!
```lisp
(load-theme 'ujelly t)
```

## Font
I like [Inconsolata](http://www.levien.com/type/myfonts/inconsolata.html), but it doesn't support a wide range of Unicode characters, so I fall back on [DejaVu Sans](http://dejavu-fonts.org/wiki/Main_Page) for those.
```lisp
(set-face-attribute 'default nil
                    :family "Inconsolata"
                    :height 130
                    :weight 'normal
                    :width 'normal)
    
(set-face-attribute 'italic nil
                    :family "DejaVu Sans Mono"
                    :height 115
                    :weight 'normal
                    :width 'normal)
    
(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                                    :size 12.4
                                    :weight 'normal)))

```
## Reduce clutter
Remove the toolbar. It's ugly and I never use it. Also remove the scroll bars and menu bar; below, I set up the fringe to show my position in a buffer.
```lisp
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)
```

## Fringe decorations
[The fringe](http://www.emacswiki.org/emacs/TheFringe) is the vertical region at the right and left of the buffer. Emacs lets you customize it of course.

Here I set up buffer position in the fringe.
```lisp
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)
```

## Mode line
Extracted from [here](<http://emacs-fu.blogspot.com.br/2011/08/customizing-mode-line.html>)
```lisp
    (setq-default mode-line-format
      (list
        ;; the buffer name; the file name as a tool tip
        '(:eval (propertize "%b " 'face 'font-lock-keyword-face
            'help-echo (buffer-file-name)))
    
        ;; line and column
        "(" ;; '%02' to set to 2 chars at least; prevents flickering
          (propertize "%02l" 'face 'font-lock-type-face) ","
          (propertize "%02c" 'face 'font-lock-type-face) 
        ") "
    
        ;; relative position, size of file
        "["
        (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
        "/"
        (propertize "%I" 'face 'font-lock-constant-face) ;; size
        "] "
    
        ;; the current major mode for the buffer.
        "["
    
        '(:eval (propertize "%m" 'face 'font-lock-string-face
                  'help-echo buffer-file-coding-system))
        "] "
    
    
        "[" ;; insert vs overwrite mode, input-method in a tooltip
        '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                  'face 'font-lock-preprocessor-face
                  'help-echo (concat "Buffer is in "
                               (if overwrite-mode "overwrite" "insert") " mode")))
    
        ;; was this buffer modified since the last save?
        '(:eval (when (buffer-modified-p)
                  (concat ","  (propertize "Mod"
                                 'face 'font-lock-warning-face
                                 'help-echo "Buffer has been modified"))))
    
        ;; is this buffer read-only?
        '(:eval (when buffer-read-only
                  (concat ","  (propertize "RO"
                                 'face 'font-lock-type-face
                                 'help-echo "Buffer is read-only"))))  
        "] "
    
        ;; add the time, with the date and the emacs uptime in the tooltip
        '(:eval (propertize (format-time-string "%H:%M")
                  'help-echo
                  (concat (format-time-string "%c; ")
                          (emacs-uptime "Uptime:%hh"))))
        " --"
        ;; i don't want to see minor-modes; but if you want, uncomment this:
        ;; minor-mode-alist  ;; list of minor modes
        "%-" ;; fill with '-'
        ))
```

# Org Mode
## General Configuration
### Clean view of files
```lisp
(defun vct-org-visual-line ()
  (setq visual-line-fringe-indicators '(nil right-curly-arrow))
  (turn-on-visual-line-mode)
)
(add-hook 'org-mode-hook 'vct-org-visual-line)
```

## LaTeX hooks

### Use CDLaTeX to enter math
```lisp
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
```

### Scale LaTeX previews
```lisp
(defun vct-org-hooks ()
    (plist-put org-format-latex-options :scale 1.4)
    (org-bullets-mode 1)
)
(add-hook 'org-mode-hook 'vct-org-hooks)
```
### Font colors for LaTeX environments
I find really useful to identify by eye LaTeX blocks.
```lisp
(defun vct-org-color-hooks ()
  (font-lock-add-keywords 'org-mode
    '(
       (
         "\\(\\\\begin\\|\\\\end\\)\\(?:\{\\)\\(.*\\)\\(?:\}\\)"
         (1 'font-lock-keyword-face)
         (2 'font-lock-function-name-face)
       )
     )
  )   
  (font-lock-add-keywords 'org-mode
    '(
       (
         "\\(\\\\label\\)\\(?:\{\\)\\(.*\\)\\(?:\}\\)"
         (1 'font-lock-keyword-face)
         (2 'font-lock-constant-face)
       )
     )
   )
)
(add-hook 'org-mode-hook 'vct-org-color-hooks)
```

## Org-Babel
### Fontifying source blocks
Enable syntax highlighting in src blocks.
```lisp
(setq-default org-src-fontify-natively t)
```