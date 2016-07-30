(defvar dotfile-which-key-delay 0.4
  "Delay in second starting from the last keystroke after which the which-key buffer will e show.")

(defvar dotfile-leader-key "SPC"
  "The leader key.")

(defvar dotfile-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar dotfile-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which the equivalent of pressing `<leader> m'.")

(defvar dotfile-major-mode-emacs-leader-key "M-,"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar my/prefix-titles nil
  "alist for mapping command prefixes to long names.")

(defvar my-default-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

(defun my/declare-prefix (prefix name &optional long-name)
  "Declare a prefix. PREFIX is a string describing a key
sequence. NAME is a string used as the prefix command. LONG-NAME
if given is stored in `my/prefix-titles'."
  (let* ((command name)
         (full-prefix (concat dotfile-leader-key " " prefix))
         (full-prefix-emacs (concat dotfile-emacs-leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
         (full-prefix-emacs-lst (listify-key-sequence
                                 (kbd full-prefix-emacs))))
    (unless long-name (setq long-name name))
    (which-key-declare-prefixes
     full-prefix-emacs (cons name long-name)
     full-prefix (cons name long-name))))

(defun my/declare-prefix-for-mode (mode prefix name &optional long-name)
  "Declare a prefix. MODE is the mode in which this prefix
command should be added. PREFIX is a string describing a key
sequence. NAME is a symbol name used as the prefix command."
  (let ((command (intern (concat (symbol-name mode) name)))
        (full-prefix (concat dotfile-leader-key " " prefix))
        (full-prefix-emacs (concat dotfile-emacs-leader-key " " prefix))
        (is-major-mode-prefix (string-prefix-p "m" prefix))
        (major-mode-prefix (concat dotfile-major-mode-leader-key
                                   " " (substring prefix 1)))
        (major-mode-prefix-emacs
         (concat dotfile-major-mode-emacs-leader-key
                 " " (substring prefix 1))))
    (unless long-name (setq long-name name))
    (let ((prefix-name (cons name long-name)))
      (which-key-declare-prefixes-for-mode mode
                                           full-prefix-emacs prefix-name
                                           full-prefix prefix-name)
      (when (and is-major-mode-prefix dotfile-major-mode-leader-key)
        (which-key-declare-prefixes-for-mode mode major-mode-prefix prefix-name))
      (when (and is-major-mode-prefix dotfile-major-mode-emacs-leader-key)
        (which-key-declare-prefixes-for-mode
         mode major-mode-prefix-emacs prefix-name)))))

(defun my/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under `dotfile-leader-key' and
`dotfile-emacs-leader-key'. KEY should be a string suitable for
passing to KBD, and it should not include the leaders. DEF is
most likely a quoted command. See `define-key' for more
information about the possible choices for DEF. This function
simply uses `define-key' to add the bindings.

For convenience, this function will accept additional KEY DEF
pairs."
  (while key
    (define-key my-default-map (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))
(put 'my/set-leader-keys 'lisp-indent-function 'defun)
(defalias 'evil-leader/set-key 'my/set-leader-keys)

(defun my//init-leader-mode-map (mode map &optional minor)
  "Check for MAP-prefix. If it doesn't exist yet, use `bind-map'
to create it and bind it to `dotfile-major-mode-leader-key' and
`dotfile-major-mode-emacs-leader-key'. If MODE is a minor-mode,
the third argument should be non-nil."
  (let* ((prefix (intern (format "%s-prefix" map)))
         (leader1 dotfile-major-mode-leader-key)
         (leader2 (concat dotfile-leader-key (unless minor " m")))
         (emacs-leader1 dotfile-major-mode-emacs-leader-key)
         (emacs-leader2 (concat dotfile-emacs-leader-key (unless minor " m")))
         (leaders (delq nil (list leader1 leader2)))
         (emacs-leaders (delq nil (list emacs-leader1 emacs-leader2))))
    (or (boundp prefix)
        (progn (eval `(bind-map ,map
                                :prefix-cmd ,prefix
                                ,(if minor :minor-modes :major-modes) (,mode)
                                :keys ,emacs-leaders
                                :evil-keys ,leaders
                                :evil-states (normal motion visual evilified)))
               (boundp prefix)))))

(defun my/set-leader-keys-for-major-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotfile-major-mode-leader-key' and
`dotfile-major-mode-emacs-leader-key' for the major-mode MODE.
MODE should be a quoted symbol corresponding to a valid major
mode. The rest of the arguments are treated exactly like they are
in `my/set-leader-keys'."
  (let* ((map (intern (format "my-%s-map" mode))))
    (when (my//init-leader-mode-map mode map)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings)
              def (pop bindings))))))
(put 'my/set-leader-keys-for-major-mode 'lisp-indent-function 'defun)
(defalias 'evil-leader/set-key-for-mode 'my/set-leader-keys-for-major-mode)

(defun my/set-leader-keys-for-minor-mode (mode key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotfile-major-mode-leader-key' and
`dotfile-major-mode-emacs-leader-key' for the minor-mode MODE.
MODE should be a quoted symbol corresponding to a valid minor
mode. The rest of the arguments are treated exactly like they are
in `my/set-leader-keys'."
  (let* ((map (intern (format "my-%s-map" mode))))
    (when (my//init-leader-mode-map mode map t)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings)
              def (pop bindings))))))
(put 'my/set-leader-keys-for-minor-mode 'lisp-indent-function 'defun)

(defun my//create-key-binding-form (props func)
  "Helper which returns a form to bind `func' to a key according to `props'.

Supported properties:

`:evil-leader STRING'
  One or several key sequence strings to be set with `my/set-leader-keys'.

`:evil-leader-for-mode CONS CELL'
  One or several cons cells (MODE . KEY) where MODE is a
  major-mode symbol and KEY is a key sequence string to be set
  with `my/set-leader-keys-for-major-Mode'.

`:global-key STRING'
  One or several key sequence strings to be set with `global-set-key'.

`:define-key CONS CELL'

  One or several cons cells (MAP . KEY) where MAP is a mode map
  and KEY is a key sequence string to be set with `define-key'."
  (let ((evil-leader (my/mplist-get props :evil-leader))
        (evil-leader-for-mode (my/mplist-get props :evil-leader-for-mode))
        (global-key (my/mplist-get props :global-key))
        (def-key (my/mplist-get props :define-key)))
    (append
     (when evil-leader
       `((dolist (key ',evil-leader)
           (my/set-leader-keys key ',func))))
     (when evil-leader-for-mode
       `((dolist (val ',evil-leader-for-mode)
           (my/set-leader-keys-for-major-mode
            (car val) (cdr val) ',func))))
     (when global-key
       `((dolist (key ',global-key)
           (global-set-key (kbd key) ',func))))
     (when def-key
       `((dolist (val ',def-key)
           (define-key (eval (car val)) (kbd (cdr val)) ',func)))))))

(defun my/basic/keybindings/init-bind-map ()
  (bind-map my-default-map
            :prefix-cmd my-emacs-cmds
            :keys (dotfile-emacs-leader-key)
            :evil-keys (dotfile-leader-key)
            :override-minor-modes t
            :override-mode-name my-leader-override-mode))

(defun my/add-which-key-description (pattern replacement)
  "Add (PATTERN . REPLACEMENT) to
`which-key-description-replacement-alist'"
  (push (cons (concat "\\`" pattern "\\'") replacement)
            which-key-description-replacement-alist))

(defun my/basic/keybindings/init-which-key ()
  (my/add-which-key-description "my/\\(.+\\)" "\\1")
  (my/add-which-key-description "select-window-\\([0-9]\\)" "window \\1")
  (my/add-which-key-description "universal-argument" "universal arg")

  (which-key-declare-prefixes
    dotfile-leader-key '("root" . "My emacs root")
    dotfile-emacs-leader-key '("root" . "My emacs root")
    (concat dotfile-leader-key " m")
    '("major-mode-cmd" . "Major mode commands")
    (concat dotfile-emacs-leader-key " m")
    '("major-mode-cmd" . "Major mode commands"))

  (which-key-setup-side-window-bottom)

  (setq which-key-special-keys nil
        which-key-use-C-h-for-paging t
        which-key-prevent-C-h-from-cycling t
        which-key-echo-keystrokes 0.02
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay dotfile-which-key-delay
        which-key-allow-evil-operators t)

  (which-key-mode)
  (my|diminish which-key-mode " K"))


(my/basic/keybindings/init-bind-map)
(my/basic/keybindings/init-which-key)
