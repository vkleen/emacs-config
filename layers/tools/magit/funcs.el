(defun my/magit-toggle-whitespace ()
  "Toggle whitespace in `magit-diff-mode'."
  (interactive)
  (if (member "-w" (if (derived-mode-p 'magit-diff-mode)
                       magit-refresh-args
                     magit-diff-section-arguments))
      (my//magit-dont-ignore-whitespace)
    (my//magit-ignore-whitespace)))

(defun my//magit-ignore-whitespace ()
  "Ignore whitespace in `magit-diff-mode'"
  (add-to-list (if (derived-mode-p 'magit-diff-mode)
                   'magit-refresh-args 'magit-diff-section-arguments) "-w")
  (magit-refresh))

(defun my//magit-dont-ignore-whitespace ()
  "Don't ignore whitespace in `magit-diff-mode'"
  (setq magit-diff-options
        (remove "-w"
                (if (derived-mode-p 'magit-diff-mode)
                    magit-refresh-args
                  magit-diff-section-arguments)))
  (magit-refresh))

(defun my//fullscreen-magit (buffer)
  "Display Magit status buffer in fullscreen."
  (if (or
       magit-display-buffer-noselect
       (and (derived-mode-p 'magit-mode)
            (not (memq (with-current-buffer buffer major-mode)
                       '(magit-process-mode
                         magit-revision-mode
                         magit-diff-mode
                         magit-stash-mode
                         magit-status-mode)))))
      (magit-display-buffer-traditional buffer)
    (delete-other-windows)
    (set-window-dedicated-p nil nil)
    (set-window-buffer nil buffer)
    (get-buffer-window buffer)))

(provide 'my//tools/magit/funcs)
