(defvar my-post-config-hook nil
  "Hook run after configuration.")
(defvar my-post-config-hook-run nil
  "Whether `my-post-config-hook' has been run.")

(defun my/init ()
  "Perform startup initialization."
  (require 'core-display-init)
;;  (hidden-mode-line-mode)
  (my//remove-gui-elements)
  (prefer-coding-system 'utf-8)
  (setq-default evil-want-C-u-scroll t
                evil-want-C-i-jump nil)
  (setq inhibit-startup-screen t
        initial-buffer-choice nil)

  (quelpa 'f)
  (quelpa 'ht)
  (quelpa 'dash)

  (require 'core-dotfile)
  (require 'core-funcs)
  (require 'core-layers)

  (my/setup-startup-hook)

  (my/discover-layers user-layer-directory)

  (quelpa 'use-package)
  (quelpa 'quelpa-use-package)
  (require 'use-package)

  (dotfile/load-file)
  (dotfile|call-func dotfile/init "Calling dotfile init...")

  (my/load-layer 'basic/ui)

  (dotfile|call-func dotfile/layers "Calling dotfile layers..."))

(defun my//remove-gui-elements ()
  "Remove the menu bar, tool bar and scroll bars."
  (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
    (menu-bar-mode -1))
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))

(defun my/setup-startup-hook ()
  "Add post init processing."
  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-hooks 'my-post-config-hook)
              (setq my-post-config-hook-run t))))

(provide 'core-init)
