(TeX-add-style-hook
 "template"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("jlreq" "uplatex" "dvipdfmx" "book")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1")))
   (TeX-run-style-hooks
    "latex2e"
    "plautopatch"
    "jlreq"
    "jlreq10"
    "amsmath"
    "amssymb"
    "amsthm"
    "empheq"
    "siunitx"
    "fontenc"
    "lmodern"
    "graphicx"
    "tikz"
    "xcolor"
    "hyperref"
    "pxjahyper")
   (TeX-add-symbols
    '("vect" 1)
    '("blue" 1)
    '("red" 1)
    '("fg" 4)
    "diff"
    "Laplace"
    "DAlambert")
   (LaTeX-add-labels
    "#4")
   (LaTeX-add-environments
    '("proof" LaTeX-env-args ["argument"] 0)
    "thm"
    "dfn"))
 :latex)

