(load "auctex.el" nil t t)

(require 'auctex-latexmk)

(setq TeX-parse-self t); Enable parse on load.
(setq TeX-auto-save t); Enable parse on save.

(setq-default TeX-master nil)
(setq TeX-PDF-mode t)

(add-hook 'TeX-mode-hook 'flyspell-mode)
(setq ispell-dictionary "english")
(add-hook 'TeX-mode-hook
          (lambda () (TeX-fold-mode 1)))
(setq LaTeX-babel-hyphen nil)

(setq LaTeX-csquotes-close-quote "}"
      LaTeX-csquotes-open-quote "\\enquote{")

(add-hook 'TeX-mode-hook 'LaTeX-math-mode)
(add-hook 'TeX-mode-hook 'turn-on-reftex)

(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)

(custom-set-variables
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "xdg-open")
     (output-html "xdg-open")))))
