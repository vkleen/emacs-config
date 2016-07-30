(unless (executable-find "agda-mode")
  (my/warning "Agda not found"))

(my/load-layer 'basic/company)

(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))
(require 'agda2-mode)

(my|use-package 'dash)
(my|use-package 's)
