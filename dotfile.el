(require 'f)

(defun dotfile/init ()
  "Initialization function."
  (my/discover-layers (f-join user-emacs-directory "private-layers"))
  (my/message "Hello World!")
  )
