(require 'cdlatex)

(defun vkleen--cdlatex-dollar (&optional arg)
  "Insert math mode delimiters."
  (interactive "P")
  (if (cdlatex-number-of-backslashes-is-odd)
      (insert "$")
    (if (texmathp)
        (progn (message "No dollars inside a math environment")
               (ding))

      (if arg
          (if (bolp)
              (progn (insert "\\[\n\n\\]\n") (backward-char 4))
            (insert "\\[  \\]") (backward-char 3))
        (insert "\\(\\)") (backward-char 2)))))

(advice-add 'cdlatex-dollar :override 'vkleen--cdlatex-dollar)

(eval-after-load "org"
  '(define-key org-cdlatex-mode-map (kbd "$") 'cdlatex-dollar))
