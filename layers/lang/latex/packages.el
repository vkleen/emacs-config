(load "auctex.el" nil t t)
(--each '(auctex-latexmk
          company-auctex
          evil-matchit
          reftex
          flyspell
          smartparens-latex)
  (my|use-package it))
