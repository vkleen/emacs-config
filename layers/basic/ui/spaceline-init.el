(defun my/customize-powerline-faces ()
  "Alter powerline face to make them work with more themes."
  (when (boundp 'powerline-inactive2)
    (set-face-attribute 'powerline-inactive2 nil
                        :inherit 'font-lock-comment-face)))

(defun my//evil-state-face ()
  (let ((state (if (eq 'operator evil-state) evil-previous-state evil-state)))
    (intern (format "my-%S-face" state))))

(defun my//restore-powerline (buffer)
  "Restore the powerline in buffer"
  (with-current-buffer buffer
    (setq-local mode-line-format (default-value 'mode-line-format))
    (powerline-set-selected-window)
    (powerline-reset)))

(defun my//set-powerline-for-startup-buffers ()
  "Set the powerline for buffers created when Emacs starts."
  (dolist (buffer '("*scratch*" "*Messages*" "*Compile-Log*"))
    (when (get-buffer buffer)
      (my//restore-powerline buffer))))

(defun my//prepare-diminish ()
  (when spaceline-minor-modes-p
    (progn (setq spaceline-minor-modes-separator "")
           (--each my--diminished-minor-modes
             (-let [(mode dim) it]
               (when (and (boundp mode) (symbol-value mode))
                 (diminish mode dim)))))))

(defun my//basic/ui/spaceline/init ()
  (require 'spaceline-config)
  (add-hook 'my-post-config-hook 'spaceline-compile)
  (setq-default powerline-default-separator nil)
  (spaceline-compile)

  (my/customize-powerline-faces)
  (setq spaceline-org-clock-p nil
        spaceline-highlight-face-func 'my//evil-state-face)
  (--each '((minor-modes "tmm")
            (major-mode "tmM")
            (version-control "tmv")
            (new-version "tmV")
            (point-position "tmp")
            (org-clock "tmc"))
    (-let* (((segment leader) it)
            (status-var (intern (format "spaceline-%S-p" segment))))
      (eval `(my|add-toggle ,(intern (format "mode-line-%S" segment))
                            :status ,status-var
                            :on (setq ,status-var t)
                            :off (setq ,status-var nil)
                            :documentation ,(format "Show %s in the mode-line."
                                                    (replace-regexp-in-string
                                                     "-" " " (format "%S" segment)))
                            :evil-leader ,leader))))

  (add-hook 'spaceline-pre-hook 'my//prepare-diminish)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode t)
  (spaceline-info-mode t)
  (my//set-powerline-for-startup-buffers)

  (setq spaceline-display-default-perspective nil))


(provide 'my//basic/ui/spaceline)
