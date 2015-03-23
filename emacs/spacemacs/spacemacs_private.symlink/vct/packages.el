(defvar vct-packages
  '(
     markdown-mode
     cdlatex
     ox-reveal
     helm-bibtex
     hydra
     key-chord
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
  :config
  (
    message "cdlatex loaded successfully!"
  )
)

(defun vct/init-ox-reveal ()
  :defer t
  :init
  (progn
    ;(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  )
  :config
  (
    message "ox-reveal loaded successfully!"
  )
)

(defun vct/init-helm-bibtex ()
  :defer t
  :init
  (progn
    ;(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  )
  :config
  (
    message "helm-bibtex loaded successfully!"
  )
)

(defun vct/init-hydra ()
  :defer t
  :init
  (progn
    ;(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  )
  :config
  (
    message "hydra loaded successfully!"
  )
)

(defun vct/init-key-chord ()
  :defer t
  :init
  (progn
    ;(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  )
  :config
  (
    message "key-chord loaded successfully!"
  )
)

