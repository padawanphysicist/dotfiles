(require 'ox-latex)

;(defun org-export-latex-no-toc (depth)
;  (when depth
;    (format "%% Org-mode is exporting headings to %s levels.\n"
;            depth)))

;(setq org-export-latex-format-toc-function 'org-export-latex-no-toc)

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
       '("revtex"
         "\\documentclass{revtex4-1}
\\usepackage[english]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{amsmath,amssymb,amsfonts,amsthm,amssymb,amsbsy,amsopn,amstext}
\\usepackage[mathcal]{eucal}
\\usepackage{mathrsfs}
\\usepackage{latexsym}
\\usepackage{bm}
\\usepackage{wrapfig}
\\usepackage{color}
\\usepackage{units}
\\usepackage{textcomp}
\\usepackage{graphicx}
\\usepackage{subfigure}
\\usepackage{hyperref}
\\usepackage{slashed}
\\usepackage{float} % Useful for right positioning of figures and tables
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]
[NO-EXTRA]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(setq org-latex-title-command "")
(setq org-latex-with-hyperref nil)

(defun vct/remove-title-date (string backend info)
  "Remove the \date{XXX} and \title{XXX} commands before the \begin{document}...\end{document}"
  (when (org-export-derived-backend-p backend 'latex)
    (message "Removing \date and \title from preamble...")
    (let ((case-fold-search nil))
      (goto-char 1)
      (replace-regexp-in-string "\\\\date{[0-9a-zA-Z\\][^}]*}" "" (replace-regexp-in-string "\\\\title{[0-9a-zA-Z][^}]*}" "" string))
    )))

(eval-after-load 'ox-latex
  '(add-to-list 'org-export-filter-final-output-functions 'vct/remove-title-date))
