(require 'my//basic/company/funcs "funcs")

(setq company-idle-delay 0.2
      company-minimum-prefix-lenght 2
      company-require-match nil
      company-dabbrev-ignore-case nil
      company-dabbrev-downcase nil)

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

(my|add-toggle auto-completion
  :status (bound-and-true-p company-mode)
  :on (progn (company-mode)
             (message "Enabled auto-completion."))
  :off (progn (company-mode -1)
              (message "Disabled auto-completion."))
  :documentation "Enable auto-completion."
  :evil-leader "ta")

(my|diminish company-mode " a")

(define-key company-active-map [return] 'nil)
(define-key company-active-map (kbd "RET") 'nil)

(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)

(define-key company-active-map (kbd "C-/") 'company-search-candidates)
(define-key company-active-map (kbd "C-M-/") 'company-filter-candidates)
(define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)

(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "C-k") 'company-select-previous)
(define-key company-active-map (kbd "C-l") 'company-complete-selection)

(setq company-transformers '(my//company-transformer-cancel
                             company-sort-by-occurrence))

(setq company-statistics-file (f-join user-cache-directory
                                      "company-statistics-cache.el"))
(add-hook 'company-mode-hook 'company-statistics-mode)

(add-hook 'company-mode-hook 'company-quickhelp-mode)
(setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))

(my/set-leader-keys "is" 'my/helm-yas)
(setq helm-c-yas-space-match-any-greedy t)

(define-key company-active-map (kbd "C-/") 'helm-company)

(global-set-key (kbd "M-/") 'hippie-expand)
(define-key evil-insert-state-map [remap evil-complete-previous] 'hippie-expnd)

(setq hippie-expand-try-functions-list '(yas-hippie-try-expand
                                         try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(setq yas-triggers-in-field t
      yas-wrap-around-region t
      helm-yas-display-key-on-candidate t
      yas-prompt-functions '(yas-completing-prompt)
      yas-minor-mode-map (make-sparse-keymap))

(define-key yas-minor-mode-map (kbd "M-S-/") 'yas-next-field)

(setq yas-snippet-dirs nil)
(push (f-join user-emacs-directory "snippets") yas-snippet-dirs)
(push (f-join user-emacs-directory "auto-snippets") yas-snippet-dirs)
(push 'yas-installed-snippets-dir yas-snippet-dirs)

(my/add-to-hooks 'my/load-yasnippet '(prog-mode-hook
                                      markdown-mode-hook
                                      org-mode-hook))

(my|add-toggle yasnippet
  :mode yas-minor-mode
  :documentation "Enable snippets."
  :evil-leader "ty")

(my/add-to-hooks 'my/force-yasnippet-off '(term-mode-hook
                                           shell-mode-hook
                                           eshell-mode-hook))

(my|diminish yas-minor-mode " y")
(setq aya-persist-snippets-dir (f-join user-emacs-directory "auto-snippets"))
(my/declare-prefix "iS" "auto-yasnippet")
(my/set-leader-keys
  "iSc" 'aya-create
  "iSe" 'my/auto-yasnippet-expand
  "iSw" 'aya-persist-snippet)

(add-hook 'yas-before-expand-snippet-hook
          (lambda ()
            (setq my//smartparens-enable-initially smartparens-mode)
            (smartparens-mode -1)))
(add-hook 'yas-after-exit-snippet-hook
          (lambda ()
            (when smartparens-enabled-initially
              (smartparens-mode 1))))
