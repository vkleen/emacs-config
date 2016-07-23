(--each '(aggressive-indent
          avy
          clean-aindent-mode
          eval-sexp-fu
          expand-region
          hexl
          hungry-delete
          link-hint
          lorem-ipsum
          move-text
          origami
          smartparens
          evil-smartparens
          undo-tree
          uuidgen
          ws-butler)
  (my|use-package it))

(require 'sp-generic-prog "local/sp-generic-prog")
(require 'smart-ops "local/smart-ops/smart-ops")
(require 'cb-buffers "local/cb-buffers")
