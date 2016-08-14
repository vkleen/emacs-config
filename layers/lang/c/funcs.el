;; Based on the Sarcasm/irony-mode compilation database code.
(defun company-mode/find-clang-complete-file ()
  (when buffer-file-name
    (let ((dir (locate-dominating-file buffer-file-name ".clang_complete")))
      (when dir
        (concat (file-name-as-directory dir) ".clang_complete")))))

;; Based on the Sarcasm/irony-mode compilation database code.
(defun company-mode/load-clang-complete-file (cc-file)
  "Load the flags from CC-FILE, one flag per line."
  (let ((invocation-dir (expand-file-name (file-name-directory cc-file)))
        (case-fold-search nil)
        compile-flags)
    (with-temp-buffer
      (insert-file-contents cc-file)
      ;; Replace relative paths with absolute paths (by @trishume)
      ;; (goto-char (point-min))
      (while (re-search-forward "\\(-I\\|-isystem\n\\)\\(\\S-\\)" nil t)
        (replace-match (format "%s%s" (match-string 1)
                               (expand-file-name (match-string 2) invocation-dir))))
      ;; Turn lines into a list
      (setq compile-flags
            ;; remove whitespaces at the end of each line, if any
            (mapcar #'(lambda (line)
                        (if (string-match "[ \t]+$" line)
                            (replace-match "" t t line)
                          line))
                    (split-string (buffer-string) "\n" t))))
    compile-flags))

(defun c/load-clang-args ()
  "Sets the arguments for company-clang, the system paths for company-c-headers
and the arguments for flyckeck-clang based on a project-specific text file."
  (unless company-clang-arguments
    (let* ((cc-file (company-mode/find-clang-complete-file))
           (flags (if cc-file (company-mode/load-clang-complete-file cc-file) '()))
           (dirs (mapcar (lambda (f) (substring f 2))
                         (remove-if-not (lambda (f) (string-prefix-p "-I" f)) flags))))
      (setq-local company-clang-arguments flags)
      (setq-local company-c-headers-path-system (append '("/usr/include" "/usr/local/include") dirs))
      (setq-local flycheck-clang-args flags))))


;; (defun cb-cpp/M-RET ()
;;   (interactive "*")
;;   (cond
;;    ((s-matches? (rx bol (* space) "using" space) (cb-buffers-current-line))
;;     (cb-cpp/semicolon-then-newline)
;;     (yas-expand-snippet "using $0;")
;;     (message "Inserted using directive."))

;;    ((s-matches? (rx bol (* space) "#include" (+ space) "<") (cb-buffers-current-line))
;;     (goto-char (line-end-position))
;;     (newline-and-indent)
;;     (yas-expand-snippet "#include <$0>")
;;     (message "Inserted library include."))

;;    ((s-matches? (rx bol (* space) "#include" (+ space) "\"") (cb-buffers-current-line))
;;     (goto-char (line-end-position))
;;     (newline-and-indent)
;;     (yas-expand-snippet "#include \"$0\"")
;;     (message "Inserted library include."))

;;    (t
;;     (cb-cpp/semicolon-then-newline)))

;;   (evil-insert-state))

;; (defun cb-cpp/semicolon-then-newline ()
;;   (goto-char (line-end-position))
;;   (delete-horizontal-space)
;;   (unless (equal ?\; (char-before))
;;     (insert ";"))
;;   (newline-and-indent))

;; (defun cb-cpp/C-RET ()
;;   (interactive "*")
;;   (goto-char (line-end-position))
;;   (delete-horizontal-space)
;;   (unless (equal ?\; (char-before))
;;     (insert ";"))
;;   (evil-insert-state))

;; (defun cb-cpp/resolve (qualified-name)
;;   "Return QUALIFIED-NAME for use in snippets.

;; If that identifier's namespace has been imported, use the name without qualification."
;;   (-let [(ns unqualified) (s-split "::" qualified-name)]
;;     (if (s-matches? (rx-to-string `(or (and bol (* space)
;;                                             "using" (+ space) "namespace" (+ space) ,ns (* space) ";")
;;                                        (and "using" (+ space) ,qualified-name (* space) ";")))
;;                     (buffer-string))
;;         unqualified
;;       qualified-name)))

(provide 'my//lang/c/funcs)
