(defvar snippet-dirs
  '("snippets"
    "packages/yasnippets-latex/snippets"))

(setq yas-snippet-dirs '())

(cl-loop for loc in snippet-dirs
         do (add-to-list 'yas-snippet-dirs
                         (concat (file-name-directory (or load-file-name
                                                          (buffer-file-name)))
                                 "../"
                                 loc)))

(yas-global-mode 1)
