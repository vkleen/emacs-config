(eval-after-load "org"
  '(progn
     (require 'ox-md nil t)
     (require 'yasnippet)))
(add-hook 'org-mode-hook 'org-cdlatex-mode)
(add-hook 'org-mode-hook '(lambda () (define-key org-cdlatex-mode-map (kbd "$") 'cdlatex-dollar)))
(add-hook 'org-mode-hook 'yas-minor-mode)
