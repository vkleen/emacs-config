(require 'my//lang/idris/funcs "funcs")

(my|defvar-company-backends idris-mode)
(my|add-company-hook idris-mode)
(push 'company-capf company-backends-idris-mode)

(put 'idris-mode 'evil-shift-width 2)

(my/declare-prefix-for-mode 'idris-mode "mb" "idris/build")
(my/declare-prefix-for-mode 'idris-mode "mi" "idris/editing")
(my/declare-prefix-for-mode 'idris-mode "mh" "idris/documentation")
(my/declare-prefix-for-mode 'idris-mode "ms" "idris/repl")
(my/declare-prefix-for-mode 'idris-mode "mm" "idris/term")

(my/set-leader-keys-for-major-mode 'idris-mode
  ;; Shorthands: rebind the standard evil-mode combinations to the local
  ;; leader for the keys not used as a prefix below.
  "c" 'idris-case-dwim
  "d" 'idris-add-clause
  "l" 'idris-make-lemma
  "p" 'idris-proof-search
  "r" 'idris-load-file
  "t" 'idris-type-at-point
  "w" 'idris-make-with-block

  ;; ipkg.
  "bc" 'idris-ipkg-build
  "bC" 'idris-ipkg-clean
  "bi" 'idris-ipkg-install
  "bp" 'idris-open-package-file

  ;; Interactive editing.
  "ia" 'idris-proof-search
  "ic" 'idris-case-dwim
  "ie" 'idris-make-lemma
  "im" 'idris-add-missing
  "ir" 'idris-refine
  "is" 'idris-add-clause
  "iw" 'idris-make-with-block

  ;; Documentation.
  "ha" 'idris-apropos
  "hd" 'idris-docs-at-point
  "hs" 'idris-type-search
  "ht" 'idris-type-at-point

  ;; Active term manipulations.
  "mn" 'idris-normalise-term
  "mi" 'idris-show-term-implicits
  "mh" 'idris-hide-term-implicits
  "mc" 'idris-show-core-term

  ;; REPL
  "'" 'idris-repl
  "sb" 'idris-load-file
  "sB" 'my/idris-load-file-and-focus
  "si" 'idris-repl
  "sn" 'idris-load-forward-line
  "sN" 'my/idris-load-forward-line-and-focus
  "sp" 'idris-load-backward-line
  "sP" 'my/idris-load-backward-line-and-focus
  "ss" 'idris-pop-to-repl)

(evil-define-key 'insert idris-mode-map
  (kbd "RET") 'idris/ret
  (kbd "M-RET") 'idris/meta-ret
;  (kbd "SPC") 'idris/smart-space
  (kbd "<backspace>") 'idris/backspace)

(evil-set-initial-state 'idris-compiler-notes-mode 'motion)
(evil-set-initial-state 'idris-hole-list-mode 'motion)
(evil-set-initial-state 'idris-info-mode 'motion)
(evil-set-initial-state 'idris-prover-script-mode 'insert)

(push '("*idris-notes*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
      popwin:special-display-config)
(push '("*idris-holes*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
      popwin:special-display-config)
(push '("*idris-info*" :dedicated t :position bottom :stick t :noselect nil :height 0.4)
      popwin:special-display-config)

(with-eval-after-load 'idris-repl
  (define-key idris-repl-mode-map (kbd "C-c C-z") 'idris/switch-to-src))

(defadvice idris-mode (before start-process activate)
  "Automatically run an idris process."
  (unless idris-process
    (idris-run)))

(defun idris/looking-at-module-or-constructor? (&rest _)
  (-when-let ([fst] (thing-at-point 'symbol))
    (s-uppercase? fst)))

(defun idris/reformat-comment-at-point ()
  (-when-let* (((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp))
               (_ (equal op "{"))
               (_ (s-matches? (rx bos "{" (* (any "-" space)) "}" eos)
                              (buffer-substring beg end))))
    (goto-char beg)
    (delete-region beg end)
    (insert "{- ") (save-excursion (insert " -}"))))

(defconst idris/smart-ops
  (-flatten-n 1
              (list
               (smart-ops "?" :pad-after nil)
               (smart-ops "," :pad-before nil)
               (smart-ops "$" "|" ":")
               (smart-ops "." :pad-unless 'idris/looking-at-module-or-constructor?)

               (smart-ops "-"
                          :action #'idris/reformat-comment-at-point)

               ;; Reformat holes after `='.
               (smart-ops "=?"
                          :pad-after nil
                          :action
                          (lambda (&rest _)
                            (save-excursion
                              (search-backward "?")
                              (just-one-space))))

               (smart-ops-default-ops))))

(define-smart-ops-for-mode 'idris-mode idris/smart-ops)
(define-smart-ops-for-mode 'idris-repl-mode idris/smart-ops)

(add-hook 'idris-mode-hook 'my/basic/toggle/smart-ops-on)
