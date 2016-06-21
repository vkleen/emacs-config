(defvar my--after-display-system-init-list '()
  "List of functions to be run after the display system is initialized.")

(defadvice server-create-window-system-frame
    (after my-init-display activate)
  "After the Emacs server creates a frame, run functions queued in `my--after-display-system-init-list' to do any setup that needs to have the display system initialized."
  (progn (dolist (fn (reverse my--after-display-system-init-list))
           (funcall fn))
         (ad-disable-advice 'server-create-window-system-frame
                            'after
                            'my-init-display)
         (ad-activate 'server-create-window-system-frame)))

(defmacro my|do-after-display-system-init (&rest body)
    "If the display-system is initialized, run `BODY', otherwise,
add it to a queue of actions to perform after the first graphical
frame is created."
    `(let ((init (cond ((boundp 'ns-initialized) ns-initialized)
                       ((boundp 'w32-initialized) (font-family-list))
                       ((boundp 'x-initialized) x-initialized)
                       (t (display-graphic-p)))))
       (if init
           (progn ,@body)
         (push (lambda () ,@body) my--after-display-system-init-list))))

(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden Mode Line Mode enabled.  "
             "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

(provide 'core-display-init)
