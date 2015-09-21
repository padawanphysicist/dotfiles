;; vct-org-hooks.el --- vct's org-mode configuration

;; Use this at your risk! I am pretty conservative with local file variables
(setq enable-local-variables :all)

(setq org-tags-column 5)

;; Different bullets for headers
(setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))

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

;; Set google translate interface
(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)
(setq google-translate-default-target-language "pt")
(setq google-translate-default-source-language "en")

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (haskell . t)
  )
)
