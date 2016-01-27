(eval-after-load "org"
  '(require 'ox-md nil t))
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
