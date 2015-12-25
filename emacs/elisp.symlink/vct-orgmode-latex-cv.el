(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
  '("vct-orgmode-latex-cv"
"\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
% Language and geometry
\\usepackage[brazil,english]{babel}
\\usepackage[top=2.5cm,bottom=2.5cm,left=2.5cm,right=2.5cm]{geometry}
% Set link colors (from http://tex.stackexchange.com/questions/100905/best-practice-for-hyperref-link-colours)
\\usepackage[dvipsnames]{xcolor}
\\usepackage{hyperref}
\\usepackage{cleveref}
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
\\usepackage{float} % Useful for right positioning of figures and tables
% Math packages
\\usepackage{amsmath,amssymb,amsfonts,amsthm}
% For include figures
\\usepackage{graphicx}
% CV formatting
\\usepackage{vct-orgmode-cv}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
  ("\\begin{flushleft}\\textcolor{red}{%s}\\end{flushleft}" . "")
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
