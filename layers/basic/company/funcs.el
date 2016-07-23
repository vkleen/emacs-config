(defvar company-mode-completion-cancel-keywords
  '("do"
    "then"
    "begin"
    "case")
  "Keywords on which to cancle completion so that you can use RET
  to complete without blocking common line endings.")

(defun my//company-complete-common-or-cycle-backward ()
  "Complete common prefix or cycle backward."
  (interactive)
  (company-complete-common-or-cycle -1))

(defun my//company-transformer-cancel (candidates)
  "Cancel completion if prefix is in the list
  `company-mode-completion-cancel-keywords'"
  (unless (member company-prefix company-mode-completion-cancel-keywords)
    candidates))

(defun my/helm-yas ()
  (interactive)
  (my/load-yasnippet)
  (call-interactively 'helm-yas-complete))

(defun my/load-yasnippet ()
  (yas-global-mode 1)
  (yas-minor-mode 1))

(defun my/force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t))

(defun my/auto-yasnippet-expand ()
  "Call `yas-expand' and switch to `insert state'"
  (interactive)
  (call-interactively 'aya-expand)
  (evil-insert-state))

(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(defmacro my|defvar-company-backends (mode)
  "Define a MODE specific company backend variable with default
  backends. The variable name format is company-backends-MODE."
  `(defvar ,(intern (format "company-backends-%S" mode))
     '((company-dabbrev-code company-gtags company-etags company-keywords)
       company-files company-dabbrev)
     ,(format "Company backend list for %S" mode)))

(defmacro my|add-company-hook (mode)
  "Enable company for the given MODE. MODE must match the symbol
  passed in `my|defvar-company-backends'. The initialization
  function is hooked to `MODE-hook'."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "my//init-company-%S" mode)))
        (backend-list (intern (format "company-backends-%S" mode))))
    `(defun ,func ()
       ,(format "Initialize company for %S" mode)
       (setq ,backend-list (mapcar 'my//show-snippets-in-company
                                   ,backend-list))
       (set (make-variable-buffer-local 'company-backends)
            ,backend-list)
       (add-hook ',mode-hook ',func t)
       (add-hook ',mode-hook 'company-mode t))))

(defmacro my|disable-company (mode)
  "Disable company for the given MODE. MODE parameter must match
  the parameter used in call to `my|add-company-hook'."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "my//init-company-%S" mode))))
    `(progn (remove-hook ',mode-hook ',func)
            (remove-hook '.mode-hook 'company-mode))))

(defun my//show-snippets-in-company (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(provide 'my//basic/company/funcs)
