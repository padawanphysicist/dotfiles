;; vct-org-hooks.el --- vct's org-mode configuration

;; Use this at your risk! I am pretty conservative with local file variables
(setq enable-local-variables :all)

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

;; This seems like a good basic set of keywords to start out with:
;(setq org-todo-keywords '((type "TODO" "NEXT" "WAITING" "DONE")))
;; Some projects need their own org files, but I still want them to
;; show up in my agenda.
;(defvar org-gtd-other-files)
;(setf org-gtd-other-files (list "~/eon/eon.org"))
;(setf org-agenda-files (cons org-gtd-file org-gtd-other-files))
;
;;; I use org's tag feature to implement contexts.
;(setq org-tag-alist '(("STUDIO" . ?s)
;		      ("COMPUTER" . ?c)
;		      ("MAIL" . ?m)
;		      ("HOME" . ?h)
;		      ("FIELD" . ?f)
;		      ("READING" . ?r)
;		      ("DVD" . ?d)))

;; I like to color-code task types.
(setf org-todo-keyword-faces '(
    ("TODO" . (:foreground "red" :bold t :weight bold))
    ("CANCELED" . (:foreground "blue" :bold t :weight bold))
    ("DONE" . (:foreground "green"))
  )
)
;(setf org-todo-keyword-faces '(("NEXT" . (:foreground "yellow" :background "red" :bold t :weight bold))
;  ("TODO" . (:foreground "cyan" :background "steelblue" :bold t :weight bold))
;  ("WAITING" . (:foreground "yellow" :background "magenta2" :bold t :weight bold))
;  ("DONE" . (:foreground "gray50" :background "gray30"))))

;; I put the archive in a separate file, because the gtd file will
;; probably already get pretty big just with current tasks.
;(setq org-archive-location "%s_archive::")

;; Remember support. This creates several files:
;;
;;   ~/todo.org      Where remembered TODO's are stored.
;;   ~/journal.org   Timestamped journal entries.
;;   ~/remember.org  All other notes

;; and a keybinding of "C-c r" for making quick notes from any buffer.

;; These bits of Remembered information must eventually be reviewed
;; and filed somewhere (perhaps in gtd.org, or in a project-specific
;; org file.) The out-of-sight, out-of-mind rule applies here---if I
;; don't review these auxiliary org-files, I'll probably forget what's
;; in them.
;(require 'remember)
;(setq org-reverse-note-order t)  ;; note at beginning of file by default.
;(setq org-default-notes-file "~/remember.org")
;(setq remember-annotation-functions '(org-remember-annotation))
;(setq remember-handler-functions '(org-remember-handler))
;(add-hook 'remember-mode-hook 'org-remember-apply-template)
;
;(setq org-remember-templates
;      '((?t "* TODO %?\n  %i\n  %a" "~/todo.org")
;        (?j "* %U %?\n\n  %i\n  %a" "~/journal.org")
;        (?i "* %^{Title}\n  %i\n  %a" "~/remember.org" "New Ideas")))
;
;(global-set-key "\C-cr" 'org-remember)
;(global-set-key [(f12)] 'org-remember)
