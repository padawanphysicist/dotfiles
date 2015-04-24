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
                                      org
                                      syntax-checking
                                      auctex
                                      git
                                      vct
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
;dotspacemacs-startup-banner nil
dotspacemacs-startup-banner 1
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
dotspacemacs-default-font '("Inconsolata-dz for Powerline" :size 14)
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
  (eval-after-load 'org
    '(progn
       (defun vct-org-visual-line ()
         (setq visual-line-fringe-indicators '(nil right-curly-arrow))
         (turn-on-visual-line-mode)
       )
       (add-hook 'org-mode-hook 'vct-org-visual-line)
       
       (defun vct-org-hooks ()
         (plist-put org-format-latex-options :scale 1.4)
       )
       (add-hook 'org-mode-hook 'vct-org-hooks)
       
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
                "\\(\\\\eqref\\)\\(?:\{\\)\\(.*\\)\\(?:\}\\)"
                (1 'font-lock-keyword-face)
                (2 'font-lock-constant-face)
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
       
       (set-face-attribute 'italic nil
                           :family "DejaVu Sans Mono"
                           ;:family "Inconsolata Italic"
                           :height 110
                           :weight 'normal
                           :width 'normal
       )
       (set-face-attribute 'variable-pitch nil :font "DejaVu Sans Mono-12" :weight 'bold)
       (dolist (face '(
                       ;org-block-begin-line
                       ;org-block-end-line
                       org-verbatim
                       org-block-background
                       org-level-1
                       org-level-2
                       org-level-3
                       org-level-4
                      )
               )
           (set-face-attribute face nil :inherit 'fixed-pitch)
       )
       (set-face-attribute 'fixed-pitch nil :font "Inconsolata-dz for Powerline")
       
       (org-babel-do-load-languages
          'org-babel-load-languages
          '((python . t)))
       (org-babel-do-load-languages
          'org-babel-load-languages
          '((maxima . t))) ; this line activates maxima
       (org-babel-do-load-languages
          'org-babel-load-languages
          '((C . t))) ; this line activates maxima

    )
  )

    (setq org-src-fontify-natively t)
  ;; Configure modeline powerline markers size
  (setq powerline-default-separator 'arrow)
  (setq-default powerline-height 26)
  
  ;; Configuration for org-ref
  (setq reftex-default-bibliography '("~/Dropbox/santos/2.documents/references/index.bib"))
  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/Dropbox/santos/2.documents/references/index.org"
        org-ref-default-bibliography '("~/Dropbox/santos/2.documents/references/index.bib")
        org-ref-pdf-directory "~/Dropbox/santos/2.documents/references/")
  (global-set-key [f10] 'org-ref-open-bibtex-notes)
  (global-set-key [f11] 'org-ref-open-bibtex-pdf)
  (global-set-key [f12] 'org-ref-open-in-browser)

  (add-to-list 'load-path "/home/santos/1.docs/org-ref")
  ;; make sure you have dash, helm, helm-bibtex, ebib, s, f, hydra and key-chord
  ;; in your load-path
  (require 'org-ref)
  
  ;; optional but very useful libraries in org-ref
  (require 'doi-utils)
  (require 'jmax-bibtex)
  (require 'pubmed)
  (require 'arxiv)
  (require 'sci-id)


  ;(defface org-block-begin-line
  ;  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  ;  "Face used for the line delimiting the begin of source blocks.")
  ;
  ;(defface org-block-background
  ;  '((t (:background "#FFFFEA")))
  ;  "Face used for the source block background.")
  ;
  ;(defface org-block-end-line
  ;  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  ;  "Face used for the line delimiting the end of source blocks.")

  (setq org-latex-create-formula-image-program 'imagemagick)
  (setq org-latex-listings 'listings) 
(setq org-latex-listings-options
        '(
          ("basicstyle" "\\color{foreground}\\footnotesize\\ttfamily")
          ("columns" "fixed")
          ("frame" "lines")
          ("numbers" "left")
          ("numberstyle" "\\tiny\\ttfamily")
          ("backgroundcolor" "\\color{background}")
          ("keywordstyle" "\\color{keyword}\\bfseries")
          ("commentstyle" "\\color{comment}\\ttfamily")
          ("identifierstyle" "\\color{identifier}")
          ("stringstyle" "\\color{string}")
         ))

     (setq org-latex-pdf-process
           '(
              "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
              "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
              "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            ))
   (setq org-latex-preview-ltxpng-directory "/home/santos/0.inbox/ltxpng/")

  (add-to-list 'load-path "/home/santos/Dropbox/santos/2.documents/config/emacs/")
(setq inferior-julia-program-name
      "/home/santos/opt/julia/julia")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) (julia . t)))
   
  (require 'ox-latex)
   (add-to-list 'org-latex-classes
         '("org-article"
"\\documentclass{article}
[NO-DEFAULT-PACKAGES]
\\usepackage[parfill]{parskip}
\\usepackage{amsmath,amssymb,amsbsy,amsfonts,amsopn,amstext,amsthm}
\\usepackage{unicode-math}
\\setmainfont[Ligatures=TeX]{Adobe Caslon Pro}
\\setmathfont{Asana Math}
\\usepackage[dvipsnames]{xcolor}
\\usepackage[xetex,colorlinks=true]{hyperref}
\\newcommand\\myshade{85}
\\colorlet{mylinkcolor}{violet}
\\colorlet{mycitecolor}{YellowOrange}
\\colorlet{myurlcolor}{Aquamarine}
\\hypersetup{
    linkcolor  = mylinkcolor!\\myshade!black,
    citecolor  = mycitecolor!\\myshade!black,
    urlcolor   = myurlcolor!\\myshade!black,
    colorlinks = true
}
\\usepackage{graphicx}
\\usepackage[top=2cm,bottom=2cm,left=2cm,right=2cm]{geometry}
\\definecolor{mygreen}{rgb}{0,0.6,0}
\\definecolor{mygray}{rgb}{0.8,0.8,0.8}
\\definecolor{mymauve}{rgb}{0.58,0,0.82}
\\definecolor{foreground}{HTML}{F8F8F2}
\\definecolor{background}{HTML}{272822}
\\definecolor{comment}{HTML}{525F58}
\\definecolor{keyword}{HTML}{F92672}
\\definecolor{string}{HTML}{E6DB74}
\\definecolor{constant}{HTML}{4E9A06}
\\definecolor{identifier}{HTML}{FD971F}
\\usepackage{listings}
\\lstloadlanguages{C++,Python}
\\lstdefinelanguage{maxima}{keywords={addrow,addcol,zeromatrix,ident,augcoefmatrix,ratsubst,diff,ev,tex,with_stdout,nouns,express,depends,load,submatrix,div,grad,curl,rootscontract,solve,print,part,assume,sqrt,facts,integrate,abs,inf,exp},sensitive=true, comment=[n][\\color{comment}\\ttfamily]{/*}{*/}}
\\lstdefinelanguage{C++}{morekeywords={diff,subs,ex,symbol,cout}}
[NO-EXTRA]"
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}" . "\\paragraph*{%s}")
            ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

)
```
