(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
  '("org-lecture-notes"
"\\documentclass[12pt,a4paper,twoside]{article}
% Font and codification
\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
% Language and geometry
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
\\usepackage{mathrsfs}
% For include figures
\\usepackage{graphicx}
% For proper hyphenation
\\usepackage{microtype}

%%%%% MAKETITLE
\\makeatletter
\\renewcommand\\maketitle{
\\begin{center}
\\coursecode \\hfill {\\Large \\coursename}\\hfill\\thisterm\\\\
\\vspace{0.2in}
\\@title\\\\
\\@author
\\end{center}
}
\\makeatother
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
