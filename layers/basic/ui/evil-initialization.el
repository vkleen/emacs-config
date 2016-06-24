(defvar dotfile-ex-substitute-global nil
  "If non nil, inverse the meaning of `g' in `:substitute' Evil
  ex-command.")

(defvar dotfile-retain-visual-state-on-shift t
  "If non-nil, the shift mappings `<' and `>' retain visual state
if used there.")

;; Shamelessly copied from spacemacs
(defvar my--indent-variable-alist
  ;; Note that derived modes must come before their sources
  '(((awk-mode c-mode c++-mode java-mode groovy-mode
      idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
    (python-mode . python-indent-offset)
    (cmake-mode . cmake-tab-width)
    (coffee-mode . coffee-tab-width)
    (cperl-mode . cperl-indent-level)
    (css-mode . css-indent-offset)
    (elixir-mode . elixir-smie-indent-basic)
    ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
    (enh-ruby-mode . enh-ruby-indent-level)
    (erlang-mode . erlang-indent-level)
    (js2-mode . js2-basic-offset)
    (js3-mode . js3-indent-level)
    ((js-mode json-mode) . js-indent-level)
    (latex-mode . (LaTeX-indent-level tex-indent-basic))
    (livescript-mode . livescript-tab-width)
    (mustache-mode . mustache-basic-offset)
    (nxml-mode . nxml-child-indent)
    (perl-mode . perl-indent-level)
    (puppet-mode . puppet-indent-level)
    (ruby-mode . ruby-indent-level)
    (rust-mode . rust-indent-offset)
    (scala-mode . scala-indent:step)
    (sgml-mode . sgml-basic-offset)
    (sh-mode . sh-basic-offset)
    (web-mode . web-mode-markup-indent-offset)
    (yaml-mode . yaml-indent-offset))
  "An alist where each key is either a symbol corresponding
to a major mode, a list of such symbols, or the symbol t,
acting as default. The values are either integers, symbols
or lists of these.")


(defun my//basic/ui/set-evil-shift-width ()
  "Set the value of `evil-shift-width' based on the indentation settings of the
current major mode."
  (let ((shift-width
         (catch 'break
           (-each my--indent-variable-alist
             (-lambda ((mode . val))
               (when (or (and (symbolp mode) (derived-mode-p mode))
                         (and (listp mode) (apply 'derived-mode-p mode))
                         (eq 't mode))
                 (when (not (listp val))
                   (setq val (list val)))
                 (dolist (v val)
                   (cond
                    ((integerp v) (throw 'break v))
                    ((and (symbolp v) (boundp v))
                     (throw 'break (symbol-value v))))))))
           (throw 'break (default-value 'evil-shift-width)))))
    (when (and (integerp shift-width)
               (< 0 shift-width))
      (setq-local evil-shift-width shift-width))))

(defun my//basic/ui/evil-smart-doc-lookup ()
  "Version of `evil-lookup' that attempts to use the mode
        specific goto-definition binding, i.e. `SPC m h h`, to
        lookup the source of the definition, while falling back
        to `evil-lookup'"
  (interactive)
  (let ((binding (key-binding (kbd (concat dotfile-leader-key " mhh")))))
    (if (commandp binding)
        (call-interactively binding)
      (evil-lookup))))

(defun my//basic/ui/evil-smart-goto-definition ()
  "Version of `evil-goto-definition' that attempts to use the
        mode specific goto-definition binding, i.e. `SPC m g g`,
        to lookup the source of the definition, while falling
        back to `evil-goto-definition'"
  (interactive)
  (let ((binding (key-binding (kbd (concat dotfile-leader-key " mgg")))))
    (if (commandp binding)
        (call-interactively binding)
      (evil-goto-definition))))

(defmacro evil-map (state key seq)
  "Map for a given STATE a KEY to a sequence SEQ of keys.

Can handle recursive definition only if KEY is the first key of
SEQ. Example: (evil-map visual \"<\" \"<gv\")"
  (let ((map (intern (format "evil-%S-state-map" state))))
    `(define-key ,map ,key
       (lambda ()
         (interactive)
         ,(if (string-equal key (substring seq 0 1))
              `(progn
                 (call-interactively ',(lookup-key evil-normal-state-map key))
                 (execute-kbd-macro ,(substring seq 1)))
            (execute-kbd-macro ,seq))))))

(defmacro my|define-text-object (key name start end)
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-outer-" name)))
        (start-regex (regexp-opt (list start)))
        (end-regex (regexp-opt (list end))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name))
       (push (cons (string-to-char ,key)
                   (if ,end
                       (cons ,start ,end)
                     ,start))
             evil-surround-pairs-alist))))

(defun my//basic/ui/init-evil ()
  (evil-mode 1)
  (defvar my-evil-cursors '(("normal" "DarkGoldenrod2" box)
                            ("insert" "chartreuse3" (bar . 2))
                            ("emacs" "SkyBlue2" box)
                            ("hybrid" "SkyBlue2" (bar . 2))
                            ("replace" "chocolate" (hbar . 2))
                            ("evilified" "LightGoldenrod3" box)
                            ("visual" "gray" (hbar . 2))
                            ("motion" "plum3" box)
                            ("lisp" "HotPink1" box)
                            ("iedit" "firebrick1" box)
                            ("iedit-insert" "firebrick1" (bar . 2)))
    "Colors assigned to evil states with cursor definitions.")
  (-each my-evil-cursors
    (-lambda ((state color cursor))
      (eval `(defface ,(intern (format "my-%s-face" state))
               `((t (list :background ,color
                          :foreground ,(face-background 'mode-line)
                          :inherit 'mode-line)))
               (format "%s state face." state)
               :group 'my-emacs))
      (set (intern (format "evil-%s-state-cursor" state))
           (list (when dotfile-colorize-cursor-according-to-state color)
                 cursor))))
  (setq evil-ex-substitute-global dotfile-ex-substitute-global)

  (setq-default evil-shift-width 2)
  (add-hook 'after-change-major-mode-hook 'my//basic/ui/set-evil-shift-width 'append)

  ;; pasting transient-state
  (evil-define-command my//transient-state-0 ()
    :keep-visual t
    :repeat nil
    (interactive)
    (if current-prefix-arg
        (progn
          (setq this-command #'digit-argument)
          (call-interactively #'digit-argument))
      (setq this-command #'evil-beginning-of-line
            hydra-deactivate t)
      (call-interactively #'evil-beginning-of-line)))

  (my|define-text-object "$" "dollar" "$" "$")
  (my|define-text-object "*" "star" "*" "*")
  (my|define-text-object "8" "block-star" "/*" "*/")
  (my|define-text-object "|" "bar" "|" "|")
  (my|define-text-object "%" "percent" "%" "%")
  (my|define-text-object "/" "slash" "/" "/")
  (my|define-text-object "_" "underscore" "_" "_")
  (my|define-text-object "-" "hyphen" "-" "-")
  (my|define-text-object "~" "tilde" "~" "~")
  (my|define-text-object "=" "equal" "=" "=")
  (evil-define-text-object evil-pasted (count &rest args)
    (list (save-excursion (evil-goto-mark ?\[) (point))
          (save-excursion (evil-goto-mark ?\]) (point))))
  ;; define text-object for entire buffer
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))

  (defadvice evil-delete-backward-char-and-join
      (around my/evil-delete-backward-char-and-join activate)
    (if (bound-and-true-p smartparens-strict-mode)
        (call-interactively 'sp-backward-delete-char)
      ad-do-it))

  (evil-escape-mode)
  (my|hide-lighter evil-escape-mode)

  (require 'evil-evilified-state "local/evil-evilified-state")
  (define-key evil-evilified-state-map (kbd dotfile-leader-key)
    my-default-map)

  (setq-default
   ediff-window-setup-function 'ediff-setup-windows-plain
   ;; emacs is evil and decrees that vertical shall henceforth be horizontal
   ediff-split-window-function 'split-window-horizontally
   ediff-merge-split-window-function 'split-window-horizontally)
  ;; restore window layout when done
  (add-hook 'ediff-quit-hook #'winner-undo)

  (evilified-state-evilify-map process-menu-mode-map :mode process-menu-mode)
  (evilified-state-evilify-map tar-mode-map :mode tar-mode))

(provide 'my//basic/ui/evil-initialization)
