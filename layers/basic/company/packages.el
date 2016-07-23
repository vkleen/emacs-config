(--each '(company
          company-quickhelp
          helm-company
          helm-c-yasnippet
          hippie-exp
          yasnippet
          auto-yasnippet)
  (my|use-package it))

(require 'company-statistics "local/company-statistics")
