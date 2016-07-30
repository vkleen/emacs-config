(require 'my//lang/haskell/funcs "funcs")

(setq haskell-process-type 'stack-ghci
      haskell-process-use-presentation-mode t
      haskell-process-show-debug-tips t
      haskell-stylish-on-save t

      haskell-indentation-layout-offset 4
      haskell-indentation-starter-offset 2
      haskell-indentation-where-pre-offset 2
      haskell-indentation-where-post-offset 2
      haskell-indentation-left-offset 4
      haskell-indent-spaces 4

      haskell-language-extensions '("-XUnicodeSyntax" "-XLambdaCase"))

(add-to-list 'completion-ignored-extensions ".hi")
(add-to-list 'completion-ignored-extensions ".gm")

(require 'haskell-debug)
(add-hook 'haskell-debug-mode-hook #'flyspell-mode-off)
(evilified-state-evilify-map haskell-debug-mode-map
  :mode haskell-debug-mode
  :bindings
  (kbd "n") #'haskell-debug/next
  (kbd "N") #'haskell-debug/previous
  (kbd "p") #'haskell-debug/previous
  (kbd "q") #'quit-window)

(require 'haskell-presentation-mode)
(evil-define-key 'normal haskell-presentation-mode-map
  (kbd "q") 'quit-window)

(evil-define-key 'normal intero-mode-map
  (kbd "M-.") 'intero-goto-definition
  (kbd "M-,") 'pop-tag-mark)

(--each '(haskell-mode
          literate-haskell-mode)
  (my/set-leader-keys-for-major-mode it
    "gg" 'intero-goto-definition
    "gG" 'pop-tag-mark
    "t"  'intero-type-at
    "d"  'intero-info
    "l"  'intero-repl-load
    "r"  'intero-apply-suggestions
    "a"  'intero-targets
    "io" 'haskell-ghc-opts-insert
    "ii" 'haskell-imports-insert-unqualified
    "iq" 'haskell-imports-insert-qualified
    "il" 'haskell-pragmas-insert)
  (my/set-leader-keys-for-major-mode it "D" 'ghc-dump-popup)
  (define-smart-ops-for-mode it
    (smart-ops "." :bypass? t)
    (smart-ops "->" "=>")
    (smart-ops "$" "=" "~" "^" ":" "?")
    (smart-ops "^." ".~" "^~" "%~" :pad-before t :pad-after t)
    (smart-op ";" :pad-before nil :pad-after t)
    (smart-op "," :pad-before nil :pad-after t)
    (smart-op "-" :action #'my//haskell/reformat-comment-at-point)
    (smart-op "#" :pad-before nil :pad-after nil :action #'my//haskell/reformat-pragma-at-point)
    (smart-ops-default-ops)))

(--each '(haskell-mode-hook
          literate-haskell-mode-hook)
  (add-hook it 'intero-mode)
  (add-hook it 'smart-ops-mode)
  (add-hook it (lambda ()
                 (setq evil-shift-width 4
                       tab-width 4))))

(bind-key "q" 'cb-buffers-maybe-kill ghc-dump-popup-mode-map)

(--each '(haskell-mode-map
          literate-haskell-mode-map)
  (evil-define-key 'insert it
    (kbd "<return>") #'haskell-ret)
  (evil-define-key 'normal it
    (kbd "<backtab>") 'haskell-indentation-indent-backwards
    (kbd "<tab>") 'haskell-indentation-indent-line))
