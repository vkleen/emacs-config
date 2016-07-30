(require 'my//lang/agda/funcs "funcs")

(mapc
 (lambda (x) (add-to-list 'face-remapping-alist x))
 '((agda2-highlight-datatype-face . font-lock-type-face)
   (agda2-highlight-function-face . font-lock-type-face)
   (agda2-highlight-inductive-constructor-face . font-lock-function-name-face)
   (agda2-highlight-keyword-face               . font-lock-keyword-face)
   (agda2-highlight-module-face                . font-lock-constant-face)
   (agda2-highlight-number-face                . font-lock-constant-face)
   (agda2-highlight-postulate-face             . font-lock-type-face)
   (agda2-highlight-primitive-type-face        . font-lock-type-face)
   (agda2-highlight-record-face                . font-lock-type-face)))

(my|define-transient-state goal-navigation
  :title "Goal Navigation Transient State"
  :doc "\n[_f_] next [_b_] previous [_q_] quit"
  :bindings
  ("f" agda2-next-goal)
  ("b" agda2-previous-goal)
  ("q" nil :exit t))
(my/set-leader-keys-for-major-mode 'agda2-mode
  "f" 'my/goal-navigation-transient-state/agda2-next-goal
  "b" 'my/goal-navigation-transient-state/agda2-previous-goal)

(my/set-leader-keys-for-major-mode 'agda2-mode
  "?"   'agda2-show-goals
  "."   'agda2-goal-and-context-and-inferred
  ","   'agda2-goal-and-context
  "="   'agda2-show-constraints
  "SPC" 'agda2-give
  "a"   'agda2-auto
  "c"   'agda2-make-case
  "d"   'agda2-infer-type-maybe-toplevel
  "e"   'agda2-show-context
  "gg"  'agda2-goto-definition-keyboard
  "gG"  'agda2-go-back
  "h"   'agda2-helper-function-type
  "l"   'agda2-load
  "n"   'agda2-compute-normalised-maybe-toplevel
  "p"   'agda2-module-contents-maybe-toplevel
  "r"   'agda2-refine
  "s"   'agda2-solveAll
  "t"   'agda2-goal-type
  "w"   'agda2-why-in-scope-maybe-toplevel
  "xc"  'agda2-compile
  "xd"  'agda2-remove-annotations
  "xh"  'agda2-display-implicit-arguments
  "xq"  'agda2-quit
  "xr"  'agda2-restart)

(define-key agda2-mode-map (kbd "M-RET") #'agda/meta-ret)

(add-to-list 'golden-ratio-exclude-buffer-names
             "*Agda information*")

(defun agda/reformat-comment-at-point ()
  (-when-let* (((&plist :beg beg :end end :op op) (sp-get-enclosing-sexp))
               (_ (equal op "{"))
               (_ (s-matches? (rx bos "{" (* (any "-" space)) "}" eos)
                              (buffer-substring beg end))))
    (goto-char beg)
    (delete-region beg end)
    (insert "{- ") (save-excursion (insert " -}"))))

(define-smart-ops-for-mode 'agda2-mode
  (smart-ops ":" "=")
  (smart-ops "-" :pad-before nil :pad-after nil :action #'agda/reformat-comment-at-point))

(defun agda/load-if-not-in-progress ()
  (unless agda2-in-progress
    (call-interactively 'agda2-load)))

(defun agda/configure-agda-mode-hooks ()
  (add-hook 'after-save-hook #'agda/load-if-not-in-progress nil t)
  (add-hook 'before-save-hook #'agda/rewrite-symbols-in-buffer nil t))
(add-hook 'agda2-mode-hook #'agda/configure-agda-mode-hooks)

(defun agda/on-goal-navigated ()
  (agda2-goal-and-context)
  (evil-insert-state))

(advice-add 'agda2-next-goal :after #'agda/on-goal-navigated)
(advice-add 'agda2-previous-goal :after #'agda/on-goal-navigated)
(--each '(agda2-refine
          agda2-give
          agda2-make-case
          agda2-auto)
  (advice-add it :after (lambda (&rest _) "Switch to evil normal mode." (evil-normal-state))))

(with-eval-after-load 'aggressive-indent
  (add-to-list 'aggressive-indent-excluded-modes 'agda2-mode))
