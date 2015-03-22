(defvar vct-packages
  '(
     markdown-mode
     cdlatex
     ox-reveal
   )
)

(defvar vct-excluded-packages
  '(
   )
)

(defun vct/init-markdown-mode ()
  (use-package markdown-mode
    :init
    (progn
    )
    :config
    (
      message "markdown-mode loaded successfully!"
    )
  )
)

(defun vct/init-cdlatex ()
  :defer t
  :init
  (progn
    (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  )
)

(defun vct/init-ox-reveal ()
  :defer t
  :init
  (progn
    ;(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  )
)


