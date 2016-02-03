(eval-after-load "org"
  '(require 'ox-md nil t)
  '(require 'yasnippet))
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
(add-hook 'org-mode-hook 'yas-minor-mode)
