(require 'core-keybindings)

(defvar my-show-debug-messages nil
  "Whether to show debug messages.")

(defun my/message (msg &rest args)
  "Display msg with (My .emacs) prepended"
  (message "(My .emacs) %s" (apply 'format msg args)))

(defun my/warning (msg &rest args)
  "Display msg as a warning message."
  (message "(My .emacs) Warning: %s" (apply 'format msg args)))

(defun my/error (msg &rest args)
  "Display msg as an error message."
  (let ((formatted (apply 'format msg args)))
    (message "(My .emacs) Error: %s" formatted)
    (error "(My .emacs) Error: %s" formatted)))

(defun my/debug-message (msg &rest args)
  "Display msg as a debug message."
  (when my-show-debug-messages
    (message "(My .emacs) %s" (apply 'format msg args))))

(defun my/run-prog-mode-hooks ()
  "Runs `prog-mode-hook'."
  (run-hooks 'prog-mode-hook))

(defun my/run-text-mode-hooks ()
  "Runs `text-mode-hook'."
  (run-hooks 'text-mode-hook))

(defun my/mplist-get (plist prop)
  "Get the values associated to `prop' in `plist', a modified plist.

A modified plist is one where keys are keywords and values are all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property with its values is returned."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun my/mplist-remove (plist prop)
  "Return a copy of a modified `plist' without `prop' and its values.

If there are multiple properties with the same keyword, only the first property and its values are removed."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (push (pop tail) result))
    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail) (not (keywordp (car tail))))
        (pop tail)))
    (while (consp tail)
      (push (pop tail) result))
    (nreverse result)))

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

(defun my/append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR, modifying the list
pointed to by LIST-VAR."
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))

(provide 'core-funcs)
