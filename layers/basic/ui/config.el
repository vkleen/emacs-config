(defconst dotfile-auto-save-directory
  (f-join user-cache-directory "auto-save")
  "My auto-save directory")

(require 'my//basic/ui/funcs "funcs")
(require 'my//basic/ui/evil-initialization "evil-initialization")
(require 'my//basic/ui/misc-options "misc-options")
(require 'my//basic/ui/themes "themes")
(require 'my//basic/ui/bindings "bindings")

(my//basic/ui/init-evil)
(my//basic/ui/misc-options)
(my//basic/ui/bindings/setup)

(my|do-after-display-system-init
   (my/message "Setting the font...")
   (unless (my/set-default-font dotfile-default-font)
     (my/warning
      "Cannot find any of the specified fonts (%s)! Font settings \
may not be correct."
      (if (listp (car dotfile-default-font))
          (mapconcat 'car dotfile-default-font ", ")
        (car dotfile-default-font)))))
