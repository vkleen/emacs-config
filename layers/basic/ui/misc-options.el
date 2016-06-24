 (defun my//basic/ui/misc-options ()
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)

  (setq dired-dwim-target t)

  (setq ring-bell-function 'ignore
        visible-bell nil)

  (add-hook 'prog-mode-hook 'goto-address-prog-mode)
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode)

  (setq help-window-select 't)

  (setq compilation-scroll-output 'first-error)

  (setq-default indent-tabs-mode nil
                tab-width 4)

  (setq longlines-show-hard-newlines t)

  (setq-default fill-column 80)
  (my|diminish auto-fill-function " F")

  (setq abbrev-file-name (f-join user-cache-directory "abbrev_defs"))

  (setq save-interprogram-paste-before-kill t)

  (setq-default sentence-end-double-space nil)

  (with-eval-after-load 'comint
    (define-key comint-mode-map (kbd "C-d") nil))

  (setq window-combination-resize t)

  (setq column-number-mode t)

  (global-hl-line-mode t)

  (blink-cursor-mode 0)

  (fset 'yes-or-no-p 'y-or-n-p)

  (setq x-underline-at-descent-line t)

  (setq minibuffer-prompt-properties
        '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

  (unless (bound-and-true-p custom-file)
    (setq custom-file (f-join user-emacs-directory "custom.el")))

  (setq initial-scratch-message nil)

  (setq make-backup-files nil)

  (setq auto-save-default t)
  (setq auto-save-list-file-prefix (concat dotfile-auto-save-directory))
  (let ((autosave-dir (f-join dotfile-auto-save-directory "site")))
    (add-to-list 'auto-save-file-name-transforms
                 `(".*" ,autosave-dir t) 'append)
    (unless (file-exists-p autosave-dir)
      (make-directory autosave-dir t)))

  (setq eval-expression-print-length nil
        eval-expression-print-level nil)

  (setq tramp-persistency-file-name (f-join user-cache-directory "tramp"))

  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)

  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-ignore-buffers-re "^\\*")

  (my|hide-lighter hi-lock-mode)

  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'text-mode-hook 'linum-mode)
  (setq linum-format "%4d")

  (global-page-break-lines-mode t)
  (my|hide-lighter page-break-lines-mode)

  (setq my-winner-boring-buffers '("*Completions*"
                                          "*Compile-Log*"
                                          "*inferior-lisp*"
                                          "*Fuzzy Completions*"
                                          "*Apropos*"
                                          "*Help*"
                                          "*cvs*"
                                          "*Buffer List*"
                                          "*Ibuffer*"
                                          "*esh command on file*"
                                          ))
  (setq winner-boring-buffers
        (append winner-boring-buffers my-winner-boring-buffers))
  (winner-mode t))

(provide 'my//basic/ui/misc-options)
