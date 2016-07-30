(--each '(async
          iedit

          evil
          evil-surround
          evil-escape
          evil-visualstar
          evil-anzu
          evil-args
          evil-exchange
          evil-iedit-state
          evil-indent-plus
          evil-lisp-state
          evil-mc
          evil-nerd-commenter
          evil-matchit
          evil-numbers
          evil-search-highlight-persist
          evil-surround
          evil-visual-mark-mode

          ediff
          evil-ediff

          uniquify
          page-break-lines
          tar-mode
          winner
          linum-relative
          vi-tilde-fringe

          ace-link
          ace-window
          buffer-move
          info+
          open-junk-file
          restart-emacs
          window-numbering

          fill-column-indicator
          golden-ratio
          hl-todo
          neotree
          popup
          popwin
          smooth-scrolling
          zoom-frm

          eyebrowse
          persp-mode

          spaceline

          undo-tree

          ace-jump-helm-line
          auto-highlight-symbol
          bookmark
          helm
          helm-ag
          helm-descbinds
          helm-flx
          helm-make
          helm-mode-manager
          projectile
          helm-projectile
          helm-swoop
          helm-themes

          auto-highlight-symbol
          column-enforce-mode
          highlight-indentation
          highlight-numbers
          highlight-parentheses
          indent-guide
          rainbow-delimiters
          volatile-highlights

          pcre2el

          unicode-fonts)
  (my|use-package it))

(require 'adaptive-wrap "local/adaptive-wrap")
