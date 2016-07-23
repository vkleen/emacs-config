(defvar dotfile-remap-Y-to-y$ nil
  "If non nil `Y' is remapped to `y$' in Evil states.")

(defvar dotfile-visual-line-move-text nil
  "If non-nil, J and K move lines up and down when in visual mode.")

(defvar dotfile-enable-paste-transient-state t
  "If non nil the paste transient-state is enabled. While enabled
pressing `p` several times cycle between the kill ring
content.'")

(defvar dotfile-emacs-command-key "SPC"
  "The key used for Emacs commands (M-x) (after pressing the leader key).")

;; We define prefix commands only for the sake of which-key
;; TODO: make this more modular
(setq my/key-binding-prefixes '(("a"   "applications")
                                ("ai"  "irc")
                                ("as"  "shells")
                                ("b"   "buffers")
                                ("bm"  "move")
                                ("c"   "compile/comments")
                                ("C"   "capture/colors")
                                ("e"   "errors")
                                ("f"   "files")
                                ("fC"  "files/convert")
                                ("fe"  "emacs")
                                ("fv"  "variables")
                                ("g"   "git/vcs")
                                ("h"   "help")
                                ("hd"  "help-describe")
                                ("i"   "insertion")
                                ("j"   "jump/join/split")
                                ("k"   "lisp")
                                ("kd"  "delete")
                                ("kD"  "delete-backward")
                                ("k`"  "hybrid")
                                ("n"   "narrow/numbers")
                                ("p"   "projects")
                                ("p$"  "projects/shell")
                                ("q"   "quit")
                                ("r"   "registers/rings/resume")
                                ("Re"  "elisp")
                                ("Rp"  "pcre")
                                ("s"   "search/symbol")
                                ("sa"  "ag")
                                ("sg"  "grep")
                                ("sk"  "ack")
                                ("st"  "pt")
                                ("sw"  "web")
                                ("t"   "toggles")
                                ("tC"  "colors")
                                ("tE"  "editing-styles")
                                ("th"  "highlight")
                                ("tm"  "modeline")
                                ("T"   "UI toggles/themes")
                                ("C-t" "other toggles")
                                ("w"   "windows")
                                ("wp"  "popup")
                                ("x"   "text")
                                ("xa"  "align")
                                ("xd"  "delete")
                                ("xj"  "justification")
                                ("xl"  "lines")
                                ("xm"  "move")
                                ("xt"  "transpose")
                                ("xw"  "words")
                                ("z"   "zoom")))

(defun my//basic/ui/bindings/setup-which-key ()
  (--each my/key-binding-prefixes (apply #'my/declare-prefix it))
  (my|add-toggle which-key
                 :mode which-key-mode
                 :documentation
                 "Display a buffer with available key bindings."
                 :evil-leader "tK")












  (my/set-leader-keys "hk" 'which-key-show-top-level)

  (dolist (leader-key `(,dotfile-leader-key ,dotfile-emacs-leader-key))
    (which-key-add-key-based-replacements
     (concat leader-key " m")    "major mode commands"
     (concat leader-key " " dotfile-emacs-command-key) "M-x")))

(defun my//basic/ui/bindings/setup-evil ()
  (customize-set-variable 'evil-want-Y-yank-to-eol dotfile-remap-Y-to-y$)

  (define-key evil-motion-state-map [C-i] 'evil-jump-forward)

  (define-key evil-normal-state-map "zf" 'reposition-window)

  (define-key evil-window-map (kbd "o") 'my/toggle-maximize-buffer)
  (define-key evil-window-map (kbd "C-o") 'my/toggle-maximize-buffer)

  (define-key evil-window-map (kbd "<left>") 'evil-window-left)
  (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  (define-key evil-window-map (kbd "<up>") 'evil-window-up)
  (define-key evil-window-map (kbd "<down>") 'evil-window-down)
  (my/set-leader-keys "re" 'evil-show-registers)

  (evil-define-key 'motion help-mode-map (kbd "<escape>") 'quit-window)
  (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
  (evil-define-key 'motion help-mode-map (kbd "S-<tab>") 'backward-button)
  (evil-define-key 'motion help-mode-map (kbd "]") 'help-go-forward)
  (evil-define-key 'motion help-mode-map (kbd "gf") 'help-go-forward)
  (evil-define-key 'motion help-mode-map (kbd "[") 'help-go-back)
  (evil-define-key 'motion help-mode-map (kbd "gb") 'help-go-back)
  (evil-define-key 'motion help-mode-map (kbd "gh") 'help-follow-symbol)

  (when dotfile-retain-visual-state-on-shift
    (evil-map visual "<" "<gv")
    (evil-map visual ">" ">gv"))

  (when dotfile-visual-line-move-text
    (define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
    (define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv")))

  (evil-ex-define-cmd "enew" 'my/new-empty-buffer)

  (define-key evil-normal-state-map (kbd "K") 'my/evil-smart-doc-lookup)
  (define-key evil-normal-state-map (kbd "gd") 'my/evil-smart-goto-definition)

  ;; scrolling transient state
  (my|define-transient-state scroll
    :title "Scrolling Transient State"
    :bindings
    ("," evil-scroll-page-up "page up")
    ("." evil-scroll-page-down "page down")
    ;; half page
    ("<" evil-scroll-up "half page up")
    (">" evil-scroll-down "half page down"))
  (my/set-leader-keys
    "n," 'my/scroll-transient-state/evil-scroll-page-up
    "n." 'my/scroll-transient-state/evil-scroll-page-down
    "n<" 'my/scroll-transient-state/evil-scroll-up
    "n>" 'my/scroll-transient-state/evil-scroll-down)

  (my|define-transient-state paste
    :title "Pasting Transient State"
    :doc "\n[%s(length kill-ring-yank-pointer)/%s(length \
 kill-ring)] [_C-j_/_C-k_] cycles through yanked text, [_p_/_P_] \
 pastes the same text above or below. Anything else exits."
    :bindings
    ("C-j" evil-paste-pop)
    ("C-k" evil-paste-pop-next)
    ("p" evil-paste-after)
    ("P" evil-paste-before)
    ("0" my//transient-state-0))

  (when dotfile-enable-paste-transient-state
    (define-key evil-normal-state-map
      "p" 'my/paste-transient-state/evil-paste-after)
    (define-key evil-normal-state-map
      "P" 'my/paste-transient-state/evil-paste-before))

  (define-key evil-inner-text-objects-map "P" 'evil-pasted)
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)

  (evil-define-key 'insert comint-mode-map
                   (kbd "C-k") 'comint-previous-input
                   (kbd "C-j") 'comint-next-input)

  (define-key evil-visual-state-map (kbd "*")
    'evil-visualstar/begin-search-forward)
  (define-key evil-visual-state-map (kbd "#")
    'evil-visualstar/begin-search-backward))

(defun my//basic/ui/bindings/setup ()
  (my//basic/ui/bindings/setup-which-key)
  (my//basic/ui/bindings/setup-evil)

  (setq echo-keystrokes 0.02)

  (define-key global-map (kbd "RET") 'newline-and-indent)

  (define-key global-map (kbd "C-x 1") 'my/toggle-maximize-buffer)

  (define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

  (define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
  (define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

  (my/set-leader-keys "u" 'universal-argument)
  (define-key universal-argument-map
    (kbd (concat dotfile-leader-key " u"))
    'universal-argument-more)

  (my/set-leader-keys "!" 'shell-command)

  (my/set-leader-keys
   "ac"  'calc-dispatch
   "ap"  'list-processes
   "aP"  'proced
   "au"  'undo-tree-visualize)

  (my/set-leader-keys
   "bd"  'my/kill-this-buffer
   "TAB" 'my/alternate-buffer
   "be"  'my/safe-erase-buffer
   "bK"  'my/kill-other-buffers
   "b C-k" 'my/kill-matching-buffers-rudely
   "bP"  'my/copy-clipboard-to-whole-buffer
   "bn"  'my/next-useful-buffer
   "bN"  'my/new-empty-buffer
   "bp"  'my/previous-useful-buffer
   "bR"  'my/safe-revert-buffer
   "bs"  'my/switch-to-scratch-buffer
   "bY"  'my/copy-whole-buffer-to-clipboard
   "bw"  'read-only-mode)

  (my/set-leader-keys
   "en" 'my/next-error
   "eN" 'my/previous-error
   "ep" 'my/previous-error)

  (my/set-leader-keys
   "fc" 'my/copy-file
   "fD" 'my/delete-current-buffer-file
   "fei" 'my/find-user-init-file
   "fed" 'my/find-dotfile
   "fg" 'rgrep
   "fl" 'find-file-literally
   "fE" 'my/sudo-edit
   "fo" 'my/open-in-external-app
   "fR" 'my/rename-current-buffer-file
   "fS" 'evil-write-all
   "fs" 'save-buffer
   "fvd" 'add-dir-local-variable
   "fvf" 'add-file-local-variable
   "fvp" 'add-file-local-variable-prop-line
   "fy" 'my/show-and-copy-buffer-filename)

  (my/set-leader-keys
   "hdb" 'describe-bindings
   "hdc" 'describe-char
   "hdf" 'describe-function
   "hdk" 'describe-key
   "hdp" 'describe-package
   "hdt" 'describe-theme
   "hdv" 'describe-variable)

  (my/set-leader-keys
   "iJ" 'my/insert-line-below-no-indent
   "iK" 'my/insert-line-above-no-indent
   "ik" 'my/evil-insert-line-above
   "ij" 'my/evil-insert-line-below)

  (my/set-leader-keys
   "jo" 'open-line
   "j=" 'my/indent-region-or-buffer
   "jk" 'my/evil-goto-next-line-and-indent)

  (my/set-leader-keys
   "j0" 'my/push-mark-and-goto-beginning-of-line
   "j$" 'my/push-mark-and-goto-end-of-line
   "jf" 'find-function-at-point
   "ji" 'my/jump-in-buffer
   "jv" 'find-variable-at-point)

  (my/set-leader-keys
   "cC" 'compile
   "ck" 'kill-compilation
   "cr" 'recompile
   "cd" 'my/close-compilation-window)
  (with-eval-after-load 'compile
    (define-key compilation-mode-map "r" 'recompile)
    (define-key compilation-mode-map "g" nil))

  (my/set-leader-keys
   "nr" 'narrow-to-region
   "np" 'narrow-to-page
   "nf" 'narrow-to-defun
   "nw" 'widen)

  (my|add-toggle line-numbers
                 :mode linum-mode
                 :documentation "Show the line numbers."
                 :evil-leader "tn")

  (my|add-toggle highlight-current-line-globally
                 :mode global-hl-line-mode
                 :documentation "Globally highlight the current line."
                 :evil-leader "thh")

  (my|add-toggle truncate-lines
                 :status truncate-lines
                 :on (toggle-truncate-lines)
                 :off (toggle-truncate-lines -1)
                 :documentation "Truncate long lines (no wrap)."
                 :evil-leader "tl")

  (my|add-toggle auto-fill-mode
                 :status auto-fill-function
                 :on (auto-fill-mode)
                 :off (auto-fill-mode -1)
                 :documentation "Break line beyond `current-fill-column` while editing."
                 :evil-leader "tF")

  (my|add-toggle debug-on-error
                 :status debug-on-error
                 :on (setq debug-on-error t)
                 :off (setq debug-on-error nil)
                 :documentation "Toggle display of backtrace when an error happens."
                 :evil-leader "tD")

  (my|add-toggle fringe
                 :status (not (equal fringe-mode 0))
                 :on (call-interactively 'fringe-mode)
                 :off (fringe-mode 0)
                 :documentation "Display the fringe in GUI mode."
                 :evil-leader "Tf")

  (my|add-toggle mode-line
                        :status (not hidden-mode-line-mode)
                        :on (hidden-mode-line-mode -1)
                        :off (hidden-mode-line-mode)
                        :documentation "Toggle the visibility of modeline."
                        :evil-leader "tmt")

  (my/set-leader-keys
   "qs" 'my/save-buffers-kill-emacs
   "qq" 'my/prompt-kill-emacs
   "qQ" 'my/kill-emacs
   "qz" 'my/frame-killer)

  (my/set-leader-keys
   "w2"  'my/layout-double-columns
   "w3"  'my/layout-triple-columns
   "wb"  'my/switch-to-minibuffer-window
   "wd"  'delete-window
   "wt"  'my/toggle-current-window-dedication
   "wf"  'follow-mode
   "wF"  'make-frame
   "wH"  'evil-window-move-far-left
   "w <S-left>"  'evil-window-move-far-left
   "wh"  'evil-window-left
   "w <left>"  'evil-window-left
   "wJ"  'evil-window-move-very-bottom
   "w <S-down>"  'evil-window-move-very-bottom
   "wj"  'evil-window-down
   "w <down>"  'evil-window-down
   "wK"  'evil-window-move-very-top
   "w <S-up>"  'evil-window-move-very-top
   "wk"  'evil-window-up
   "w <up>"  'evil-window-up
   "wL"  'evil-window-move-far-right
   "w <S-right>"  'evil-window-move-far-right
   "wl"  'evil-window-right
   "w <right>"  'evil-window-right
   "wm"  'my/toggle-maximize-buffer
   "wM"  'my-centered-buffer-mode
   "wo"  'other-frame
   "wr"  'my/rotate-windows
   "wR"  'my/rotate-windows-backward
   "ws"  'split-window-below
   "wS"  'my/split-window-below-and-focus
   "w-"  'split-window-below
   "wU"  'winner-redo
   "wu"  'winner-undo
   "wv"  'split-window-right
   "wV"  'my/split-window-right-and-focus
   "ww"  'other-window
   "w/"  'split-window-right
   "w="  'balance-windows)

  (defalias 'count-region 'count-words-region)

  (my/set-leader-keys
   "xa&" 'my/align-repeat-ampersand
   "xa(" 'my/align-repeat-left-paren
   "xa)" 'my/align-repeat-right-paren
   "xa," 'my/align-repeat-comma
   "xa." 'my/align-repeat-decimal
   "xa:" 'my/align-repeat-colon
   "xa;" 'my/align-repeat-semicolon
   "xa=" 'my/align-repeat-equal
   "xa\\" 'my/align-repeat-backslash
   "xaa" 'align
   "xac" 'align-current
   "xam" 'my/align-repeat-math-oper
   "xar" 'my/align-repeat
   "xa|" 'my/align-repeat-bar
   "xc"  'count-region
   "xdw" 'delete-trailing-whitespace
   "xjc" 'set-justification-center
   "xjf" 'set-justification-full
   "xjl" 'set-justification-left
   "xjn" 'set-justification-none
   "xjr" 'set-justification-right
   "xls" 'my/sort-lines
   "xlu" 'my/uniquify-lines
   "xtc" 'transpose-chars
   "xtl" 'transpose-lines
   "xtw" 'transpose-words
   "xU"  'upcase-region
   "xu"  'downcase-region
   "xwc" 'my/count-words-analysis)

  (with-eval-after-load 'shell
    (evil-define-key 'insert comint-mode-map [up] 'comint-previous-input)
    (evil-define-key 'insert comint-mode-map [down] 'comint-next-input))

  (my|define-transient-state buffer
                             :title "Buffer Selection Transient State"
                             :bindings
                             ("n" my/next-useful-buffer "next")
                             ("N" my/previous-useful-buffer "previous")
                             ("p" my/previous-useful-buffer "previous")
                             ("K" my/kill-this-buffer "kill")
                             ("q" nil "quit" :exit t))
  (my/set-leader-keys "b." 'my/buffer-transient-state/body)

  (my|define-transient-state window-manipulation
                             :title "Window Manipulation Transient State"
                             :doc "
 Select^^^^              Move^^^^              Split^^                Resize^^                     Other^^
 ──────^^^^───────────── ────^^^^───────────── ─────^^─────────────── ──────^^──────────────────── ─────^^──────────────────────────────
 [_j_/_k_] down/up       [_J_/_K_] down/up     [_s_] vertical         [_[_] shrink horizontally    [_q_] quit
 [_h_/_l_] left/right    [_H_/_L_] left/right  [_S_] vert & follow    [_]_] enlarge horizontally   [_u_] restore prev layout
 [_0_-_9_] window N      [_R_]^^   rotate      [_v_] horizontal       [_{_] shrink vertically      [_U_] restore next layout
 [_w_]^^   other window  ^^^^                  [_V_] horiz & follow   [_}_] enlarge vertically     [_d_] close current
 [_o_]^^   other frame   ^^^^                  ^^                     ^^                           [_D_] close other
 ^^^^                    ^^^^                  ^^                     ^^                           [_g_] golden-ratio %`golden-ratio-mode"
                             :bindings
                             ("q" nil :exit t)
                             ("0" select-window-0)
                             ("1" select-window-1)
                             ("2" select-window-2)
                             ("3" select-window-3)
                             ("4" select-window-4)
                             ("5" select-window-5)
                             ("6" select-window-6)
                             ("7" select-window-7)
                             ("8" select-window-8)
                             ("9" select-window-9)
                             ("-" my/split-window-below-and-focus)
                             ("/" my/split-window-right-and-focus)
                             ("[" my/shrink-window-horizontally)
                             ("]" my/enlarge-window-horizontally)
                             ("{" my/shrink-window)
                             ("}" my/enlarge-window)
                             ("d" delete-window)
                             ("D" delete-other-windows)
                             ("g" my/basic/toggle/golden-ratio)
                             ("h" evil-window-left)
                             ("<left>" evil-window-left)
                             ("j" evil-window-down)
                             ("<down>" evil-window-down)
                             ("k" evil-window-up)
                             ("<up>" evil-window-up)
                             ("l" evil-window-right)
                             ("<right>" evil-window-right)
                             ("H" evil-window-move-far-left)
                             ("<S-left>" evil-window-move-far-left)
                             ("J" evil-window-move-very-bottom)
                             ("<S-down>" evil-window-move-very-bottom)
                             ("K" evil-window-move-very-top)
                             ("<S-up>" evil-window-move-very-top)
                             ("L" evil-window-move-far-right)
                             ("<S-right>" evil-window-move-far-right)
                             ("o" other-frame)
                             ("R" my/rotate-windows)
                             ("s" split-window-below)
                             ("S" my/split-window-below-and-focus)
                             ("u" winner-undo)
                             ("U" winner-redo)
                             ("v" split-window-right)
                             ("V" split-window-right-and-focus)
                             ("w" other-window))
  (my/set-leader-keys "w." 'my/window-manipulation-transient-state/body)

  (my|define-transient-state scale-font
                             :title "Font Scaling Transient State"
                             :doc "\n[_+_/_=_] scale up [_-_] scale down [_0_] reset font [_q_] quit"
                             :bindings
                             ("+" my/scale-up-font)
                             ("=" my/scale-up-font)
                             ("-" my/scale-down-font)
                             ("0" my/reset-font-size)
                             ("q" nil :exit t))
  (my/set-leader-keys "zx" 'my/scale-font-transient-state/body)

  (global-set-key (kbd "<f20>") 'my/backward-capitalize-word))

(provide 'my//basic/ui/bindings)
