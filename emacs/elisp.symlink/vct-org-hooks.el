;; vct-org-hooks.el --- vct's org-mode configuration

;; Use this at your risk! I am pretty conservative with local file variables
(setq enable-local-variables :all)

(setq org-tags-column 5)

;; LaTeX math highlighting in org buffers
(font-lock-add-keywords 'org-mode
  '(
    (
      "\\(\\\\begin\\|\\\\end\\)\\(?:\{\\)\\(.*\\)\\(?:\}\\)"
      (1 'font-lock-keyword-face)
      (2 'font-lock-function-name-face)
    )
    (
      "\\(\\\\eqref\\|\\\\ref\\|\\\\href\\|\\\\label\\)\\(?:\{\\)\\(.*\\)\\(?:\}\\)"
      (1 'font-lock-keyword-face)
      (2 'font-lock-constant-face)
    )
    ("\\(\\\\textrm\\|\\\\frac\\)" (1 'font-lock-keyword-face))
  )
)

;; Bigger LaTeX previews
(plist-put org-format-latex-options :scale 1.4)

;; Change ltxpng folder location for LaTeX previews
(setq org-latex-preview-ltxpng-directory "~/.ltxpng/")

;; Change default org mode faces for bold and emphasis
(add-to-list 'org-emphasis-alist '("*" (:foreground "#EEEEEE" :weight bold)))
(add-to-list 'org-emphasis-alist '("/" (:family "DejaVu Sans Mono" :size 12 :slant oblique)))

;; Set default font for tags
(custom-set-faces
  '(org-tag ((t (:foreground "DarkOrange3" :background "gray13" :box t))))
)

; Set google translate interface
(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)
(setq google-translate-default-target-language "pt")
(setq google-translate-default-source-language "en")

;; CDLaTex configuration
(setq cdlatex-env-alist '(("vct-eqn" "\\begin{equation}\n?\n\\end{equation}\n" nil)))

(setq cdlatex-command-alist '(("eqn" "Insert non-numbered env"   "" cdlatex-environment ("vct-eqn") t nil)))

;; Make my life easier when typesetting tensors using abstract index notation
(setq cdlatex-math-symbol-alist '((?p ("\\phantom{?}"))))

(require 'ox-publish)
(setq org-publish-project-alist
    '(("org" :components ("org-notes" "org-static"))
        ("org-notes"
         :publishing-function org-html-publish-to-html
         :headline-levels 4 ; Default for this project
         :auto-sitemap t ; Generate sitemap.org automagically...
         :sitemap-filename "sitemap.org" ; ...call it `sitemap.org`...
         :sitemap-title "Sitemap" ; ...with title `Sitemap`
         :export-creator-info t ; Include `Created by Org` in the postamble
         :export-author-info t ; Include `Author: Your name` in the postamble
         :html-postamble nil ; enable postamble
         :html-preamble nil
         :base-directory "~/public_html/org"
         :base-extension "org"
         :publishing-directory "~/public_html/"
         :recursive t
         :section-numbers nil
         :with-toc nil
         :with-drawers t
         :style-include-default t  ;Disable the default css style
         )
        ("org-static"
         :base-directory "~/public_html/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/"
         :recursive t
         :publishing-function org-publish-attachment
         :section-numbers nil
         )
    )
)
