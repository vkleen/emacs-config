(my/load-layer 'basic/company)

(quelpa `(let-alist :fetcher file :path ,(f-join (f-dirname (f-this-file)) "local/let-alist.el") :version original))
(quelpa `(seq :fetcher file :path ,(f-join (f-dirname (f-this-file)) "local/seq") :version original))

(--each '(haskell-mode
          intero
          llvm-mode
          asm-mode
          magit-popup)
  (my|use-package it))

(require 'ghc-dump "local/ghc-dump")
(require 'haskell-ghc-opts "local/haskell-ghc-opts")
(require 'haskell-imports "local/haskell-imports")
(require 'haskell-pragmas "local/haskell-pragmas")
(require 'haskell-ret "local/haskell-ret")
