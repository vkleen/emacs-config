(defvar my-post-theme-init-hook nil
  "Hook to be run after a theme is loaded.")

(defadvice load-theme (after my/load-theme-adv activate)
  "Perform post load processing."
  (let ((theme (ad-get-arg 0)))
    ;; Without this a popup is raised every time emacs25 starts up for
    ;; assignment to a free variable
    (with-no-warnings
      (setq my--cur-theme theme))
    (run-hooks 'my-post-theme-init-hook)))

(defun my/load-theme (theme)
  "Load THEME."
  (mapc 'disable-theme custom-enabled-themes)
  (if (eq 'default theme)
        (setq my--cur-theme 'default)
    (load-theme theme t)
    ;; explicitly reload the theme for the first GUI client
    (eval `(my|do-after-display-system-init
            (load-theme ',theme t)))))

(provide 'my//basic/ui/themes)
