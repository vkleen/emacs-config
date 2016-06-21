(defvar my/prefix-titles nil
  "alist for mapping command prefixes to long names.")

(defvar my-default-map (make-sparse-keymap)
  "Base keymap for all leader key commands.")

(defun my/declare-prefix (prefix name &optional long-name)
  "Declare a prefix. `prefix' is a string describing a key sequence. `name' is a string used as the prefix command. `long-name' if given is stored in `my/prefix-titles'."
  (let* ((command name)
         (full-pefix (concat dotfile-leader-key " " prefix))
         (full-prefix-emacs (concat dotfile-emacs-leader-key " " prefix))
         (full-prefix-lst (listify-key-sequence (kbd full-prefix)))
         (full-prefix-emacs-lst (listify-key-sequence
                                 (kbd full-prefix-emacs))))
    (unless long-name (setq long-name name))
    (which-key-declare-prefixes
     full-prefix-emacs (cons name long-name)
     full-prefix (cons name long-name))))

(defun my/declare-prefix-for-mode (mode prefix name &optional long-name)
  "Declare a prefix. `mode' is the mode in which this prefix command should be added. `prefix' is a string describing a key sequence. `name' is a symbol name used as the prefix command."
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
  "Add `key' and `def' as key bindings under `dotfile-leader-key' and `dotfile-emacs-leader-key'. `key' should be a string suitable for passing to `kbd', and it should not include the leaders. `def' is most likely a quoted command. See `define-key' for more information about the possible choices for `def'. This function simply uses `define-key' to add the bindings.

For convenience, this function will accept additional `key' `def' pairs."
  (while key
    (define-key my-default-map (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))
(put 'my/set-leader-keys 'lisp-indent-function 'defun)
(defalias 'evil-leader/set-key 'my/set-leader-keys)

(defun my//init-leader-mode-map (mode map &optional minor)
  "Check for `map'-prefix. If it doesn't exist yet, use `bind-map' to create it and bind it to `dotfile-major-mode-leader-key' and `dotfile-major-mode-emacs-leader-key'. If `mode' is a minor-mode, the third argument should be non-nil."
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
  "Add `key' and `def' as key bindings under `dotfile-major-mode-leader-key' and `dotfile-major-mode-emacs-leader-key' for the major-mode `mode'. `mode' should be a quoted symbol corresponding to a valid major mode. The rest of the arguments are treated exactly like they are in `my/set-leader-keys'."
  (let* ((map (intern (format "my-%s-map" mode))))
    (when (my//init-leader-mode-map mode map)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings)
              def (pop bindings))))))
(put 'my/set-leader-keys-for-major-mode 'lisp-indent-function 'defun)
(defalias 'evil-leader/set-key-for-mode 'my/set-leader-keys-for-major-mode)

(defun my/set-leader-keys-for-minor-mode (mode key def &rest bindings)
  "Add `key' and `def' as key bindings under `dotfile-major-mode-leader-key' and `dotfile-major-mode-emacs-leader-key' for the minor-mode `mode'. `mode' should be a quoted symbol corresponding to a valid minor mode. The rest of the arguments are treated exactly like they are in `my/set-leader-keys'."
  (let* ((map (intern (format "my-%s-map" mode))))
    (when (my//init-leader-mode-map mode map t)
      (while key
        (define-key (symbol-value map) (kbd key) def)
        (setq key (pop bindings)
              def (pop bindings))))))
(put 'my/set-leader-keys-for-minor-mode 'lisp-indent-function 'defun)

(provide 'core-keybindings)
