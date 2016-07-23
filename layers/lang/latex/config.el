(require 'my//lang/latex/funcs "funcs")

(setq TeX-command-default latex-build-command
      TeX-auto-save t
      TeX-parse-self t
      TeX-syntactic-comment t
      TeX-source-correlate-start-server nil
      LaTeX-fill-break-at-separators nil)

(when latex-enable-auto-fill
  (add-hook 'LaTeX-mode-hook 'latex/auto-fill-mode))
(when latex-enable-folding
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode))
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

(-each '(tex-mode latex-mode)
  (lambda (mode)
    (my/set-leader-keys-for-major-mode mode
      "\\" 'TeX-insert-macro
      "-" 'TeX-recenter-output-buffer
      "%" 'TeX-comment-or-unomment-paragraph
      ";" 'TeX-comment-or-uncomment-region
      "b" 'latex/build
      "k" 'TeX-kill-job
      "l" 'TeX-recenter-output-buffer
      "m" 'TeX-insert-macro
      "v" 'TeX-view
      "hd" 'TeX-doc

      "xb" 'latex/font-bold
      "xc" 'latex/font-code
      "xe" 'latex/font-emphasis
      "xi" 'latex/font-italic
      "xr" 'latex/font-clear
      "xo" 'latex/font-oblique
      "xfc" 'latex/font-small-caps
      "xff" 'latex/font-sans-serif
      "xfr" 'latex/font-serif)
    (my/set-leader-keys-for-major-mode mode
      dotfile-major-mode-emacs-leader-key 'TeX-command-master)
    (my/set-leader-keys-for-major-mode mode
      dotfile-major-mode-leader-key 'TeX-command-master)
    (my/set-leader-keys-for-major-mode mode
      "z=" 'TeX-fold-math
      "zb" 'TeX-fold-buffer
      "ze" 'TeX-fold-env
      "zm" 'TeX-fold-macro
      "zr" 'TeX-fold-region)
    (my/declare-prefix-for-mode mode "mh" "help")
    (my/declare-prefix-for-mode mode "mx" "text/fonts")
    (my/declare-prefix-for-mode mode "mz" "fold")))

(my/set-leader-keys-for-major-mode 'latex-mode
  "*" 'LaTeX-mark-section
  "." 'LaTeX-mark-environment
  "c" 'LaTeX-close-environment
  "e" 'LaTeX-environment
  "ii" 'LaTeX-insert-item
  "s" 'LaTeX-section
  "fe" 'LaTeX-fill-environment
  "fp" 'LaTeX-fill-paragraph
  "fr" 'LaTeX-fill-region
  "fs" 'LaTeX-fill-section
  "pb" 'preview-buffer
  "pc" 'preview-clearout
  "pd" 'preview-document
  "pe" 'preview-environment
  "pf" 'preview-cache-preamble
  "pp" 'preview-at-point
  "pr" 'preview-region
  "ps" 'preview-section
  "xB" 'latex/font-medium
  "xr" 'latex/font-clear
  "xfa" 'latex/font-calligraphic
  "xfn" 'latex/font-normal
  "xfu" 'latex/font-upright)

(my/declare-prefix-for-mode 'latex-mode "mi" "insert")
(my/declare-prefix-for-mode 'latex-mode "mp" "preview")
(my/declare-prefix-for-mode 'latex-mode "mf" "fill")

(setq auctex-latexmk-inherit-TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook 'auctex-latexmk-setup)

(my|defvar-company-backends LaTeX-mode)
(my|add-company-hook LaTeX-mode)

(push 'company-auctex-labels company-backends-LaTeX-mode)
(push 'company-auctex-bibs company-backends-LaTeX-mode)
(push '(company-auctex-macros
        company-auctex-symbols
        company-auctex-environments) company-backends-LaTeX-mode)

(add-hook 'LaTeX-mode-hook 'evil-matchit-mode)
;;(my/add-flycheck-hook 'LaTeX-mode)
;;(my/add-flyspell-hook 'LaTeX-mode-hook))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX '(nil nil t t t)
      reftex-use-fonts t)
(my/declare-prefix-for-mode 'latex-mode "mr" "reftex")
(my/set-leader-keys-for-major-mode 'latex-mode
  "rc" 'reftex-citation
  "rg" 'reftex-grep-document
  "ri" 'reftex-index-selection-or-word
  "rI" 'reftex-display-index
  "r TAB" 'reftex-index
  "rl" 'reftex-label
  "rp" 'reftex-index-phrase-selection-or-word
  "rP" 'reftex-index-visit-phrases-buffer
  "rr" 'reftex-reference
  "rs" 'reftex-search-document
  "rt" 'reftex-toc
  "rT" 'reftex-toc-recenter
  "rv" 'reftex-view-crossref)

(eval-after-load "latex"
  '(define-key LaTeX-mode-map (kbd "\"") nil))

(add-hook 'LaTeX-mode-hook 'smartparens-mode)
(add-hook 'LaTeX-mode-hook 'my/load-yasnippet)

(push '("\\`latex-font-\\(.+\\)\\'" . "\\1")
      which-key-description-replacement-alist)

(defun my//fixup-smartparens-latex ()
  (sp-with-modes '(latex-mode)
    (sp-local-pair "\\(" "\\)"
                   :trigger "$"
                   :unless '(sp-latex-point-after-backslash))
    (sp-local-pair "``" "''" :actions nil :trigger nil)
    (sp-local-pair "\\enquote{" "}"
                   :trigger "\""
                   :unless '(sp-latex-point-after-backslash))
    (sp-local-tag "\"" "\\enquote{" "}" :actions '(wrap))))
(eval-after-load "smartparens-latex" '(my//fixup-smartparens-latex))

(custom-set-variables
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "xdg-open")
     (output-html "xdg-open")))))
