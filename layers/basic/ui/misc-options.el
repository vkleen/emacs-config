(defun my//basic/ui/misc-options ()
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)

  (setq dired-dwim-target t)

  (setq ring-bell-function 'ignore
        visible-bell nil)

  (add-hook 'prog-mode-hook 'goto-address-prog-mode)
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode)

  (setq help-window-select 't)

  (setq compilation-scroll-output 'first-error)

  (setq-default indent-tabs-mode nil
                tab-width 4)

  (setq longlines-show-hard-newlines t)

  (setq-default fill-column 80)
  (my|diminish auto-fill-function " F")

  (setq abbrev-file-name (f-join user-cache-directory "abbrev_defs"))

  (setq save-interprogram-paste-before-kill t)

  (setq-default sentence-end-double-space nil)

  (with-eval-after-load 'comint
    (define-key comint-mode-map (kbd "C-d") nil))

  (setq window-combination-resize t)

  (setq column-number-mode t)

  (global-hl-line-mode t)

  (blink-cursor-mode 0)

  (fset 'yes-or-no-p 'y-or-n-p)

  (setq x-underline-at-descent-line t)

  (setq minibuffer-prompt-properties
        '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

  (unless (bound-and-true-p custom-file)
    (setq custom-file (f-join user-emacs-directory "custom.el")))

  (setq initial-scratch-message nil)

  (setq make-backup-files nil)

  (setq auto-save-default t)
  (setq auto-save-list-file-prefix (concat dotfile-auto-save-directory))
  (let ((autosave-dir (f-join dotfile-auto-save-directory "site")))
    (add-to-list 'auto-save-file-name-transforms
                 `(".*" ,autosave-dir t) 'append)
    (unless (file-exists-p autosave-dir)
      (make-directory autosave-dir t)))

  (setq eval-expression-print-length nil
        eval-expression-print-level nil)

  (setq tramp-persistency-file-name (f-join user-cache-directory "tramp"))

  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)

  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-ignore-buffers-re "^\\*")

  (my|hide-lighter hi-lock-mode)

  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'text-mode-hook 'linum-mode)
  (setq linum-format "%4d")

  (global-page-break-lines-mode t)
  (my|hide-lighter page-break-lines-mode)

  (setq my-winner-boring-buffers '("*Completions*"
                                   "*Compile-Log*"
                                   "*inferior-lisp*"
                                   "*Fuzzy Completions*"
                                   "*Apropos*"
                                   "*Help*"
                                   "*cvs*"
                                   "*Buffer List*"
                                   "*Ibuffer*"
                                   "*esh command on file*"))
  (setq winner-boring-buffers
        (append winner-boring-buffers my-winner-boring-buffers))
  (winner-mode t)

  (linum-relative-on)
  (my/set-leader-keys "tr" 'linum-relative-toggle)
  (setq linum-relative-current-symbol "")

  (with-eval-after-load 'info
    (define-key Info-mode-map "o" 'ace-link-info))
  (with-eval-after-load 'help-mode
    (define-key help-mode-map "o" 'ace-link-help))

  (my/set-leader-keys
   "bM" 'ace-swap-window
   "wD" 'ace-delete-window
   "w SPC" 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  (my/set-leader-keys
   "bmh" 'buf-move-left
   "bmj" 'buf-move-down
   "bmk" 'buf-move-up
   "bml" 'buf-move-right)

  (setq Info-fontify-angle-bracketed-flag nil)

  (setq open-junk-file-format (f-join user-cache-directory "junk/%Y/%m/%d-%H%M%S"))
  (my/set-leader-keys "fJ" 'my/open-junk-file)

  (my/set-leader-keys
   "qd" 'my/restart-emacs-debug-init
   "qr" 'my/restart-emacs-resume-layouts
   "qR" 'my/restart-emacs)

  (defun window-numbering-install-mode-line (&optional position)
    "Do nothing, the display is handled by powerline.")
  (setq window-numbering-auto-assign-0-to-minibuffer nil)
  (my/set-leader-keys
   "0" 'select-window-0
   "1" 'select-window-1
   "2" 'select-window-2
   "3" 'select-window-3
   "4" 'select-window-4
   "5" 'select-window-5
   "6" 'select-window-6
   "7" 'select-window-7
   "8" 'select-window-8
   "9" 'select-window-9)
  (window-numbering-mode 1)

  (setq window-numbering-assign-func
        (lambda ()
          (when (and (boundp 'neo-buffer-name)
                     (string= (buffer-name) neo-buffer-name))
            0)))

  (setq fci-rule-width 1
        fci-rule-collor "#D0BF8F")
  (push '(fci-mode "") minor-mode-alist)
  (my|add-toggle fill-column-indicator
                 :status fci-mode
                 :on (turn-on-fci-mode)
                 :off (turn-off-fci-mode)
                 :documentation "Display the fill column indicator"
                 :evil-leader "tf")
  (my|diminish fci-mode " f")

  (my|add-toggle golden-ratio
                 :status golden-ratio-mode
                 :on (golden-ratio-mode) (golden-ratio)
                 :off (golden-ratio-mode -1) (balance-windows)
                 :documentation "Resize the focused window using the golden ratio."
                 :evil-leader "tg")
  (--each '("bs-mode"
            "calc-mode"
            "ediff-mode"
            "dired-mode"
            "gud-mode"
            "gdb-locals-mode"
            "gdb-registers-mode"
            "gdb-breakpoints-mode"
            "gdb-threads-mode"
            "gdb-frames-mode"
            "gdb-inferior-io-mode"
            "gdb-disassembly-mode"
            "gdb-memory-mode"
            "speedbar-mode")
    (add-to-list 'golden-ratio-exclude-modes it))
  (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")

  (--each '(ace-window
            ace-delete-window
            ace-select-window
            ace-swap-window
            ace-maximize-window
            avy-pop-mark
            buf-move-left
            buf-move-right
            buf-move-up
            buf-move-down
            evil-avy-goto-word-or-subword-1
            evil-avy-goto-line
            evil-window-delete
            evil-window-split
            evil-window-vsplit
            evil-window-left
            evil-window-right
            evil-window-up
            evil-window-down
            evil-window-bottom-right
            evil-window-top-left
            evil-window-mru
            evil-window-next
            evil-window-prev
            evil-window-new
            evil-window-vnew
            evil-window-rotate-upwards
            evil-window-rotate-downwards
            evil-window-move-very-top
            evil-window-move-far-left
            evil-window-move-far-right
            evil-window-move-very-bottom
            quit-window
            select-window-0
            select-window-1
            select-window-2
            select-window-3
            select-window-4
            select-window-5
            select-window-6
            select-window-7
            select-window-8
            select-window-9
            windmove-left
            windmove-right
            windmove-up
            windmove-down)
    (add-to-list 'golden-ratio-extra-commands it))

  (--each '(" *NeoTree*"
            "*LV*"
            " *which-key*")
    (add-to-list 'golden-ratio-exclude-buffer-names it))

  (add-to-list 'golden-ratio-inhibit-functions
               'my/no-golden-ratio-guide-key)
  (my|diminish golden-ratio-mode " g")

  (my/add-to-hooks 'hl-todo-mode '(text-mode-hook
                                   prog-mode-hook))

  (setq neo-window-width 32
        neo-create-file-auto-open t
        neo-banner-message nil
        neo-show-updir-line nil
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-dont-be-alone t
        neo-persist-show nil
        neo-show-hidden-files t
        neo-auto-indent-point t
        neo-modern-sidebar t
        neo-vc-integration nil)
  (evilified-state-evilify-map neotree-mode-map
                               :mode neotree-mode
                               :bindings
                               (kbd "TAB")  'neotree-stretch-toggle
                               (kbd "RET") 'neotree-enter
                               (kbd "|") 'neotree-enter-vertical-split
                               (kbd "-") 'neotree-enter-horizontal-split
                               (kbd "?") 'evil-search-backward
                               (kbd "c") 'neotree-create-node
                               (kbd "d") 'neotree-delete-node
                               (kbd "gr") 'neotree-refresh
                               (kbd "h") 'my/neotree-collapse-or-up
                               (kbd "H") 'neotree-select-previous-sibling-node
                               (kbd "J") 'neotree-select-down-node
                               (kbd "K") 'neotree-select-up-node
                               (kbd "l") 'my/neotree-expand-or-open
                               (kbd "L") 'neotree-select-next-sibling-node
                               (kbd "q") 'neotree-hide
                               (kbd "r") 'neotree-rename-node
                               (kbd "R") 'neotree-change-root
                               (kbd "s") 'neotree-hidden-file-toggle)
  (my/set-leader-keys
   "ft" 'neotree-toggle
   "pt" 'neotree-find-project-root)

  (popwin-mode 1)
  (my/set-leader-keys "wpm" 'popwin:messages)
  (my/set-leader-keys "wpp" 'popwin:close-popup-window)

  ;; don't use default value but manage it ourselves
  (setq popwin:special-display-config nil)

  ;; buffers that we manage
  (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
  (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '(" *undo-tree*"           :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
  (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
  (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)

  (setq scroll-preserve-screen-position t
        scroll-margin 5
        scroll-conservatively 101)
  (my|add-toggle smooth-scrolling
                 :status (= 101 scroll-conservatively)
                 :on (my/enable-smooth-scrolling)
                 :off (my/disable-smooth-scrolling)
                 :documentation "Smooth scrolling"
                 :evil-leader "tv")

  (my|define-transient-state zoom-frm
                             :title "Zoom Frame Transient State"
                             :doc "[_+_/_=_] zoom frame in [_-_] zoom frame out [_0_] reset zoom [_q_] quit"
                             :bindings
                             ("+" my/zoom-frm-in)
                             ("=" my/zoom-frm-in)
                             ("-" my/zoom-frm-out)
                             ("0" my/zoom-frm-unzoom)
                             ("q" nil :exit t))
  (my/set-leader-keys "zf" 'my/zoom-frm-transient-state/body)
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  (my|hide-lighter undo-tree-mode)

  (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode)

  (setq ahs-case-fold-search nil
        ahs-default-range 'ahs-range-whole-buffer
        ahs-idle-timer 0
        ahs-idle-interval 0.25
        ahs-inhibit-face-list nil
        my--symbol-highlight-transient-state-doc
            "
 %s  [_n_] next  [_N_/_p_] previous        [_r_] change range   [_R_] reset       [_e_] iedit
 %s  [_d_/_D_] next/previous definition")

  (setq auto-highlight-symbol-mode-map (make-sparse-keymap))
  (my|add-toggle automatic-symbol-highlight
                 :status (timerp ahs-idle-timer)
                 :on (progn (auto-highlight-symbol-mode)
                            (setq ahs-idle-timer
                                  (run-with-idle-timer ahs-idle-interval t
                                                       'ahs-idle-function)))
                 :off (when (timerp ahs-idle-timer)
                        (auto-highlight-symbol-mode)
                        (cancel-timer ahs-idle-timer)
                        (setq ahs-idle-timer 0))
                 :documentation "Automatic highlight of current symbol."
                 :evil-leader "tha")
  (my/add-to-hooks 'auto-highlight-symbol-mode '(prog-mode-hook
                                                 markdown-mode-hook))
  (my|hide-lighter auto-highlight-symbol-mode)
  (define-key evil-motion-state-map (kbd "*") 'my/enter-ahs-forward)
  (define-key evil-motion-state-map (kbd "#") 'my/enter-ahs-backward)
  (my/set-leader-keys "sh" 'my/symbol-highlight
                      "sH" 'my/goto-last-search-ahs-symbol)

  (dolist (sym '(ahs-forward
                 ahs-forward-definition
                 ahs-backward
                 ahs-backward-definition
                 ahs-back-to-start
                 ahs-change-range))
    (let* ((advice (intern (format "my/%s" (symbol-name sym)))))
      (eval `(defadvice ,sym (around ,advice activate)
               (my/ahs-highlight-now-wrapper)
               ad-do-it
               (my/ahs-highlight-now-wrapper)
               (setq my-last-ahs-highlight-p (ahs-highlight-p))))))
  (my|define-transient-state symbol-highlight
                             :title "Symbol Highlight Transient State"
                             :dynamic-hint (my//symbol-highlight-ts-doc)
                             :before-exit (my//ahs-ms-on-exit)
                             :bindings
                             ("d" ahs-forward-definition)
                             ("D" ahs-backward-definition)
                             ("e" ahs-to-iedit :exit t)
                             ("n" my/quick-ahs-forward)
                             ("N" my/quick-ahs-backward)
                             ("p" my/quick-ahs-backward)
                             ("R" ahs-back-to-start)
                             ("r" ahs-change-range)
                             ("q" nil :exit T))
  (my|add-toggle highlight-long-lines
        :status column-enforce-mode
        :prefix columns
        :on (column-enforce-n (or columns column-enforce-column))
        :on-message (format "long-lines enabled for %s columns." (or columns column-enforce-column))
        :off (column-enforce-mode -1)
        :documentation "Highlight the characters past the 80th column."
        :evil-leader "t8")
  (my|add-toggle highlight-long-lines-globally
        :mode global-column-enforce-mode
        :documentation "Globally Highlight the characters past the 80th column."
        :evil-leader "t C-8")
  (my|diminish column-enforce-mode "8")

  (my|add-toggle highlight-indentation
                 :mode highlight-indentation-mode
                 :documentation "Highlight indentation levels."
                 :evil-leader "thi")
  (my|add-toggle highlight-indentation-current-column
                 :mode highlight-indentation-current-column-mode
                 :documentation "Highlight indentation level at point."
                 :evil-leader "thc")

  (my|diminish highlight-indentation-mode " hi")
  (my|diminish highlight-indentation-current-column-mode " hc")

  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1)))

  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  (setq hl-paren-delay 0.2)
  (my/set-leader-keys "tCp" 'highlight-parentheses-mode)
  (setq hl-paren-colors '("Springgreen3"
                          "IndianRed1"
                          "IndianRed3"
                          "IndianRed4"))
  (my|hide-lighter highlight-parentheses-mode)
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-Bold)

  (setq indent-guide-delay 0.3)
  (my|add-toggle indent-guide
                 :mode indent-guide-mode
                 :documentation
                 "Highlight indentation level at point. (alternative to highlight-indentation)."
                 :evil-leader "ti")
  (my|add-toggle indent-guide-globally
                 :mode indent-guide-global-mode
                 :documentation
                 "Highlight indentation level at point globally. (alternative to highlight-indentation)."
                 :evil-leader "t TAB")
  (my|diminish indent-guide-mode " i")

  (my/set-leader-keys "tCd" 'rainbow-delimiters-mode)
  (my/add-to-hooks 'rainbow-delimiters-mode '(prog-mode-hook))

  (vhl/define-extension 'evil
                        'evil-move
                        'evil-paste-after
                        'evil-paste-before
                        'evil-paste-pop)
  (vhl/install-extension 'evil)
  (vhl/load-extension 'evil)
  ;; undo-tree
  (vhl/define-extension 'undo-tree
                        'undo-tree-move
                        'undo-tree-yank)
  (vhl/install-extension 'undo-tree)
  (vhl/load-extension 'undo-tree)
  (volatile-highlights-mode)
  (my|hide-lighter volatile-highlights-mode)

  (my/declare-prefix "xr" "regular expressions")
  (my/declare-prefix "xre" "elisp")
  (my/declare-prefix "xrp" "pcre")
  (my/set-leader-keys "xr/"  'rxt-explain
                      "xr'"  'rxt-convert-to-strings
                      "xrt"  'rxt-toggle-elisp-rx
                      "xrx"  'rxt-convert-to-rx
                      "xrc"  'rxt-convert-syntax
                      "xre/" 'rxt-explain-elisp
                      "xre'" 'rxt-elisp-to-strings
                      "xrep" 'rxt-elisp-to-pcre
                      "xret" 'rxt-toggle-elisp-rx
                      "xrex" 'rxt-elisp-to-rx
                      "xrp/" 'rxt-explain-pcre
                      "xrp'" 'rxt-pcre-to-strings
                      "xrpe" 'rxt-pcre-to-elisp
                      "xrpx" 'rxt-pcre-to-rx))

(provide 'my//basic/ui/misc-options)
