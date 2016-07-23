(defun dotfile/init ()
  "Initialization function."
  (my/message "Hello World!"))

(defun dotfile/layers ()
  "Load layers and do miscellaneous configuration."
  (my/load-layer 'themes/solarized)
  (my/load-theme 'solarized-dark)

  (my/load-layer 'basic/company)
  (my/load-layer 'lang/idris)
  (my/load-layer 'lang/latex)

  (my/basic/toggle/smartparens-globally-on)
  (smartparens-strict-mode 1)

  (my/basic/toggle/auto-completion-on))
