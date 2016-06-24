(defun dotfile/init ()
  "Initialization function."
  (my/message "Hello World!")
  )

(defun dotfile/layers ()
  "Load layers and do miscellaneous configuration."
  (my/load-layer 'themes/solarized)
  (my/load-theme 'solarized-dark))
