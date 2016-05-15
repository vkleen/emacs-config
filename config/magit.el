(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(defun magit-status--fullscreen (orig-fun &rest args)
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fun args)
  (delete-other-windows))

(defun magit-quit-window--restore-screen ()
  (jump-to-register :magit-fullscreen))

(advice-add 'magit-status :around 'magit-status--fullscreen)
(advice-add 'magit-quit-window :after 'magit-quit-window--restore-screen)
