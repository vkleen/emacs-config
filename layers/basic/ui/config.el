(defconst dotfile-auto-save-directory
  (f-join user-cache-directory "auto-save")
  "My auto-save directory")

(require 'my//basic/ui/funcs "funcs")
(require 'my//basic/ui/evil-initialization "evil-init")
(require 'my//basic/ui/misc-options "misc-options")
(require 'my//basic/ui/themes "themes")
(require 'my//basic/ui/bindings "bindings")
(require 'my//basic/ui/helm "helm-config")
(require 'my//basic/ui/spaceline "spaceline-init")
(require 'my//basic/ui/layouts "layouts")

(my//basic/ui/init-evil)
(my//basic/ui/helm/init)
(my//basic/ui/misc-options)
(my//basic/ui/bindings/setup)

(my//basic/ui/spaceline/init)

(my//basic/ui/layouts/init)

(my/message "Switching to scratch buffer")
(my/switch-to-scratch-buffer)

(my|do-after-display-system-init
   (my/message "Setting the font...")
   (unicode-fonts-setup)
   (unless (my/set-default-font dotfile-default-font)
     (my/warning
      "Cannot find any of the specified fonts (%s)! Font settings \
may not be correct."
      (if (listp (car dotfile-default-font))
          (mapconcat 'car dotfile-default-font ", ")
        (car dotfile-default-font)))))
