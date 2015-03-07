;;; extensions.el --- vct Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar vct-pre-extensions
  '(
     ;; pre extension vcts go here
   )
  ; "List of all extensions to load before the packages."
)

(defvar vct-post-extensions
  '(
    ;; post extension vcts go here
    org
    )
  ; "List of all extensions to load after the packages."
)

(defun vct/init-org ()
  (use-package org
    :init
    (progn
    )
    :config
    (progn
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
               "\\(\\\\label\\)\\(?:\{\\)\\(.*\\)\\(?:\}\\)"
               (1 'font-lock-keyword-face)
               (2 'font-lock-constant-face)
             )
           )
         )
      )
      (add-hook 'org-mode-hook 'vct-org-color-hooks)

      (set-face-attribute 'italic nil
                          ;;:family "DejaVu Sans Mono"
                          :family "Inconsolata Italic"
                          :height 100
                          :weight 'normal
                          :width 'normal)
      ;(set-face-attribute 'variable-pitch nil :font "DejaVu Sans Mono-11" :weight 'bold)
      (dolist (face '(
                      org-block-begin-line
                      org-block-end-line
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
      (set-face-attribute 'fixed-pitch nil :font "Inconsolata-11")
    )
  )
)

;; For each extension, define a function vct/init-<extension-vct>
;;
;; (defun vct/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
