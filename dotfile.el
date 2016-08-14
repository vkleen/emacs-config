(defun dotfile/init ()
  "Initialization function."
  (my/message "Hello World!")
  (setq dotfile-auto-resume-layouts nil))

(defun dotfile/layers ()
  "Load layers and do miscellaneous configuration."
  (my/load-layer 'themes/solarized)
  (my/load-theme 'solarized-dark)

  (my/load-layer 'basic/company)

  (my/load-layer 'lang/idris)
  (my/load-layer 'lang/latex)
  (my/load-layer 'lang/agda)
  (my/load-layer 'lang/haskell)
  (my/load-layer 'lang/c)

  (my/load-layer 'tools/magit)

  (my/basic/toggle/smartparens-globally-on)
  (smartparens-strict-mode 1)
  (require 'nix-mode))
