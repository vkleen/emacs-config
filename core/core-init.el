(defun my/init ()
  "Perform startup initialization."
  (require 'core-display-init)
  (hidden-mode-line-mode)
  (my//remove-gui-elements)
  (prefer-coding-system 'utf-8)
  (setq-default evil-want-C-u-scroll t
                evil-want-C-i-jump nil)
  (quelpa 'f)
  (quelpa 'ht)
  (quelpa 'dash)
  (quelpa 'quelpa-use-package)

  (require 'core-dotfile)
  (require 'core-fonts)
  (require 'core-diminish)
  (require 'core-keybindings)
  (require 'core-funcs)
  (require 'core-layers)

  (my/discover-layers user-layer-directory)

  (dotfile/load-file)
  (dotfile|call-func dotfile/init "Calling dotfile init...")
  (my|do-after-display-system-init
   (my/message "Setting the font...")
   (unless (my/set-default-font dotfile-default-font)
     (my/warning
      "Cannot find any of the specified fonts (%s)! Font settings may not be correct."
      (if (listp (car dotfile-default-font))
          (mapconcat 'car dotfile-default-font ", ")
        (car dotfile-default-font)))))
  (setq inhibit-startup-screen t
        initial-buffer-choice nil))

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

(defun display-startup-echo-area-message ()
  "Change the default welcome message."
  (message "We are ready."))

(provide 'core-init)
