(my/load-layer 'basic/company)
(my/load-layer 'tags/ggtags)
(my/load-layer 'tags/cscope)

(--each '(cc-mode
          disaster
          clang-format
          cmake-mode
          company
          company-c-headers
          flycheck
          gdb-mi
          helm-cscope
          eldoc
          ggtags
          helm-gtags
          semantic
          stickyfunc-enhance
          xcscope
          srefactor
          dash-functional)
  (my|use-package it))

(require 'cpp-autoinsert "local/cpp-autoinsert")
(require 'compile)
