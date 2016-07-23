(require 'my//basic/editing/funcs "funcs")

(my|add-toggle aggressive-indent
  :mode aggressive-indent-mode
  :documentation "Always keep code indented."
  :evil-leader "tI")
(my|add-toggle aggressive-indent-globally
  :mode aggressive-indent-mode
  :documentation "Always keep code indented globally."
  :evil-leader "t C-I")
(add-hook 'diff-auto-refine-mode-hook 'my/toggle-aggressive-indent-off)
(my|diminish aggressive-indent-mode " I")

(setq avy-all-windows 'all-frames)
(setq avy-background t)
(my/set-leader-keys "jb" 'avy-pop-mark
                    "jj" 'evil-avy-goto-char
                    "jJ" 'evil-avy-goto-char-2
                    "jl" 'evil-avy-goto-line
                    "ju" 'my/avy-goto-url
                    "jw" 'evil-avy-goto-word-or-subword-1
                    "xo" 'my/avy-open-url)

(add-hook 'prog-mode-hook 'clean-aindent-mode)

(let ((byte-compile-not-obsolete-funcs
       (append byte-compile-not-obsolete-funcs '(preceding-sexp))))
  (require 'eval-sexp-fu))

(my/set-leader-keys "v" 'er/expand-region)
(defadvice er/prepare-for-more-expansions-internal
    (around helm-ag/prepare-for-more-expansions-internal activate)
  ad-do-it
  (let ((new-msg (concat (car ad-return-value)
                         ", / to search in project, "
                         "f to search in files, "
                         "b to search in opened buffers"))
        (new-bindings (cdr ad-return-value)))
    (cl-pushnew
     '("/" (lambda ()
             (call-interactively
              'my/helm-project-smart-do-search-region-or-symbol)))
     new-bindings)
    (cl-pushnew
     '("f" (lambda ()
             (call-interactively
              'my/helm-files-smart-do-search-region-or-symbol)))
     new-bindings)
    (cl-pushnew
     '("b" (lambda ()
             (call-interactively
              'my/helm-buffers-smart-do-search-region-or-symbol)))
     new-bindings)
    (setq ad-return-value (cons new-msg new-bindings))))
(setq expand-region-contract-fast-key "V"
      expand-region-reset-fast-key "r")

(my/set-leader-keys "fh" 'hexl-find-file)
(my/set-leader-keys-for-major-mode 'hexl-mode
                                   "d" 'hexl-insert-decimal-char
                                   "c" 'hexl-insert-octal-char
                                   "x" 'hexl-insert-hex-char
                                   "X" 'hexl-insert-hex-string
                                   "g" 'hexl-goto-address)
(evil-define-key 'motion hexl-mode-map
                 "]]" 'hexl-end-of-1k-page
                 "[[" 'hexl-beginning-of-1k-page
                 "h" 'hexl-backward-char
                 "l" 'hexl-forward-char
                 "j" 'hexl-next-line
                 "k" 'hexl-previous-line
                 "$" 'hexl-end-of-line
                 "^" 'hexl-beginning-of-line
                 "0" 'hexl-beginning-of-line)

(my|add-toggle hungry-delete
               :mode hungry-delete-mode
               :documentation "Delete consecutive horizontal whitespace with a single key."
               :evil-leader "td")
(setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace
(define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
(define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char)

(my/set-leader-keys "xo" 'link-hint-open-link
                    "xO" 'link-hint-open-multiple-links)

(my/declare-prefix "il" "lorem ipsum")
(my/set-leader-keys "ill" 'lorem-ipsum-insert-list
  "ilp" 'lorem-ipsum-insert-paragraphs
  "ils" 'lorem-ipsum-insert-sentences)

(my|define-transient-state move-text
  :title "Move Text Transient State"
  :bindings
  ("J" move-text-down "move down")
  ("K" move-text-up "move up"))
(my/set-leader-keys
 "xJ" 'my/move-text-transient-state/move-text-down
 "xK" 'my/move-text-transient-state/move-text-up)

(global-origami-mode)
(define-key evil-normal-state-map "za" 'origami-forward-toggle-node)
(define-key evil-normal-state-map "zc" 'origami-close-node)
(define-key evil-normal-state-map "zC" 'origami-close-node-recursively)
(define-key evil-normal-state-map "zO" 'origami-open-node-recursively)
(define-key evil-normal-state-map "zo" 'origami-open-node)
(define-key evil-normal-state-map "zr" 'origami-open-all-nodes)
(define-key evil-normal-state-map "zm" 'origami-close-all-nodes)
(define-key evil-normal-state-map "zs" 'origami-show-only-node)
(define-key evil-normal-state-map "zn" 'origami-next-fold)
(define-key evil-normal-state-map "zp" 'origami-previous-fold)
(define-key evil-normal-state-map "zR" 'origami-reset)
(define-key evil-normal-state-map (kbd "z <tab>") 'origami-recursively-toggle-node)
(define-key evil-normal-state-map (kbd "z TAB") 'origami-recursively-toggle-node)

(my|define-transient-state fold
  :title "Code Fold Transient State"
  :doc "
 Close^^            Open^^             Toggle^^         Goto^^         Other^^
 ───────^^───────── ─────^^─────────── ─────^^───────── ──────^^────── ─────^^─────────
 [_c_] at point     [_o_] at point     [_a_] at point   [_n_] next     [_s_] single out
 [_C_] recursively  [_O_] recursively  [_A_] all        [_p_] previous [_R_] reset
 [_m_] all          [_r_] all          [_TAB_] like org ^^             [_q_] quit"
  :foreign-keys run
  :on-enter (unless (bound-and-true-p origami-mode) (origami-mode 1))
  :bindings
  ("a" origami-forward-toggle-node)
  ("A" origami-toggle-all-nodes)
  ("c" origami-close-node)
  ("C" origami-close-node-recursively)
  ("o" origami-open-node)
  ("O" origami-open-node-recursively)
  ("r" origami-open-all-nodes)
  ("m" origami-close-all-nodes)
  ("n" origami-next-fold)
  ("p" origami-previous-fold)
  ("s" origami-show-only-node)
  ("R" origami-reset)
  ("TAB" origami-recursively-toggle-node)
  ("<tab>" origami-recursively-toggle-node)
  ("q" nil :exit t)
  ("C-g" nil :exit t)
  ("<SPC>" nil :exit t))

(my/add-to-hooks 'smartparens-mode
                 '(prog-mode-hook comint-mode-hook))

;; enable smartparens-mode in `eval-expression'
(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

(my|add-toggle smartparens
  :mode smartparens-mode
  :documentation "Enable smartparens."
  :evil-leader "tp")

(my|add-toggle smartparens-globally
  :mode smartparens-mode
  :documentation "Enable smartparens globally."
  :evil-leader "t C-p")

(setq sp-show-pair-delay 0.2
      ;; fix paren highlighting in normal mode
      sp-show-pair-from-inside t
      sp-cancel-autoskip-on-backward-movement nil)

(my/set-leader-keys "js" 'sp-split-sexp
                    "jn" 'sp-newline)

(require 'smartparens-config)
(my|diminish smartparens-mode " p")

(show-smartparens-global-mode +1)

(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(sp-local-pair 'org-mode                 "'" "'" :actions '(:rem insert))
(sp-local-pair 'extempore-mode           "'" "'" :actions '(:rem insert))
(sp-local-pair 'text-mode                "'" "'" :actions '(:rem insert))
(sp-local-pair 'minibuffer-inactive-mode "'" "'" :actions '(:rem insert))

;; don't create a pair with single quote in minibuffer
(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

(sp-pair "{" nil :post-handlers
         '(:add (my/smartparens-pair-newline-and-indent "RET")))
(sp-pair "[" nil :post-handlers
         '(:add (my/smartparens-pair-newline-and-indent "RET")))

(define-key evil-insert-state-map ")" 'my/smart-closing-parenthesis)

(my|hide-lighter smart-ops-mode)
(smart-ops-init)

(my|add-toggle smart-ops
  :mode smart-ops-mode
  :documentation "Smart-ops mode."
  :evil-leader "to")

(my/declare-prefix "iU" "uuid")
(my/set-leader-keys "iU1" 'my/uuidgen-1
                    "iU4" 'my/uuidgen-4
                    "iUU" 'my/uuidgen-4)

(ws-butler-global-mode)
(my|hide-lighter ws-butler-mode)
