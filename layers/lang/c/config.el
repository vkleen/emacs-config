(require 'my//lang/c/funcs "funcs")

(defvar c-c++-default-mode-for-headers 'c-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'.")

;;; HACK: add a dedicated semantic layer?
(setq srecode-map-save-file (concat user-cache-directory
                                    "srecode-map.el"))
(setq semanticdb-default-save-directory (concat user-cache-directory
                                                "semanticdb/"))
(unless (file-exists-p semanticdb-default-save-directory)
  (make-directory semanticdb-default-save-directory))

(add-to-list 'semantic-default-submodes
                   'global-semantic-stickyfunc-mode)
      (add-to-list 'semantic-default-submodes
                   'global-semantic-idle-summary-mode)

(my|defvar-company-backends c-mode-common)
(my|defvar-company-backends cmake-mode)

(add-to-list 'auto-mode-alist `("\\.h\\'" . ,c-c++-default-mode-for-headers))
(c-toggle-auto-newline 1)
(--each '(c-mode c++-mode)
  (my/set-leader-keys-for-major-mode it
    "ga" 'projectile-find-other-file
    "gA" 'projectile-find-other-file-other-window
    "gi" 'cscope-index-files
    "D" 'disaster
    "r" 'srefactor-refactor-at-point)

  (push it flycheck-global-modes)

  (my/helm-gtags-define-keys-for-mode it)
  (my/setup-helm-cscope it))

(push 'company-cmake company-backends-cmake-mode)

(my|add-company-hook c-mode-common)
(my|add-company-hook cmake-mode)

(push 'company-clang company-backends-c-mode-common)

(push 'company-c-headers company-backends-c-mode-common)

(setq company-clang-prefix-guesser (lambda ()
                                     (c/load-clang-args)
                                     (company-clang--prefix)))
(my/add-to-hooks 'c/load-clang-args '(c-mode-hook c++-mode-hook))
(my/add-to-hooks 'my/ggtags-mode-enable '(c-mode-hook c++-mode-hook))

(setq gdb-many-windows t
      gdb-show-main t)

(my/add-to-hooks 'semantic-mode '(c-mode-hook c++-mode-hook))
(add-hook 'srefactor-ui-menu-mode-hook 'evil-emacs-state)

;; (my|defvar-company-backends c-mode-common)
;; (my|add-company-hook c-mode-common)

;; (add-hook 'c++-mode-hook #'irony-mode)

;; (add-hook 'irony-mode-hook (lambda ()
;;                              (define-key irony-mode-map [remap completion-at-point] #'irony-completion-at-point-async)
;;                              (define-key irony-mode-map [remap complete-symbol] #'irony-completion-at-point-async)))
;; (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

;; (setq irony-user-dir (f-join user-emacs-directory "irony")
;;       irony-server-install-prefix irony-user-dir
;;       irony-additional-clang-options '("-std=c++14"))

;; (push 'compay-irony company-backends-c-mode-common)
;; (add-hook 'irony-mode-hook #'company-irony-setup-begin-commands)

;; (add-hook 'irony-mode-hook #'irony-eldoc)

;; (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)

;; (push 'company-irony-c-headers company-backends-c-mode-common)

;; (setq-default clang-format-style "Google")
;; (add-hook 'c-mode-common-hook #'google-set-c-style)

;; (add-hook 'c++-mode-hook #'ggtags-mode)
;; (set-face-underline 'ggtags-highlight nil)

;; (add-hook 'c++-mode-hook #'helm-gtags-mode)

;; (add-to-list 'aggressive-indent-excluded-modes 'c++-mode)

;; (defconst c++-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key   map "#" #'c-electric-pound)
;;     (define-key map "\C-c\C-e" #'c-macro-expand)
;;     (define-key map (kbd "M-RET") #'cb-cpp/M-RET)
;;     (define-key map (kbd "C-<return>") #'cb-cpp/C-RET)
;;     map))

;; (evil-define-key 'insert c++-mode-map
;;   (kbd "<backspace>") #'sp-generic-prog-backspace
;;   (kbd "SPC") #'sp-generic-prog-space)

;; (defun cb-cpp/set-local-hooks ()
;;   (add-hook 'before-save-hook #'indent-dwim-whole-buffer nil t))

;; (add-hook 'c++-mode-hook #'cb-cpp/set-local-hooks)

;; (font-lock-add-keywords
;;  'c++-mode
;;  `((";" 0 font-lock-comment-face t)
;;    ("\\_<constexpr\\_>" 0 font-lock-keyword-face t)
;;    ("\\_<noexcept\\_>" 0 font-lock-keyword-face t)))

;; (defun cb-cpp/flyspell-verify ()
;;   "Do not spellcheck imports."
;;   (and (flyspell-generic-progmode-verify)
;;        (not (s-matches? (rx bol (* space) "#") (cb-buffers-current-line)))))

;; (defun cb-cpp/configure-flyspell ()
;;   (setq-local flyspell-generic-check-word-predicate 'cb-cpp/flyspell-verify))

;; (add-hook 'c++-mode-hook 'cb-cpp/configure-flyspell)

(defun cb-cpp/after-operator-keyword? (&rest _)
  (save-excursion
    (goto-char (smart-ops--maybe-beginning-of-op (smart-ops--rules-for-current-mode)))
    (thing-at-point-looking-at (rx bow "operator" eow (* space)))))

(--each '(c++-mode
          c-mode)
  (define-smart-ops-for-mode it
    (smart-ops
     "+" "-" "/" "%" "^" "|" "!" "=" "<<" ">>" "==" "!=" "&&" "||"
     "+=" "-=" "/=" "%=" "^=" "|=" "*=" "<<=" ">>=" "?" "&" "*"
     :pad-unless 'cb-cpp/after-operator-keyword?)
    (smart-ops "*>" "*>&" ">&" :pad-before nil :pad-after nil
               :action
               (lambda ()
                 (skip-chars-forward "*>&")))
    (smart-ops ":"
               :pad-unless
               (-orfn 'cb-cpp/after-operator-keyword?
                      (smart-ops-after-match? (rx (or "public" "private" "protected")))))
    (smart-ops "," :pad-before nil :pad-unless 'cb-cpp/after-operator-keyword?)
    (smart-ops ";" :pad-before nil)
    (smart-ops "--" "++" :pad-before nil :pad-after nil)
    (smart-ops "." "::" "->" "->*" ">::"
               :pad-before nil :pad-after nil
               :action 'company-manual-begin)

    (smart-op "<>"
              :pad-before nil :pad-after nil
              :action (lambda (&rest _) (search-backward ">")))))

;; (cpp-autoinsert-init)
