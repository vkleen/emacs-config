(defun my/uuidgen-1 (arg)
  "Return a time based UUID (UUIDv1).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-1)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))

(defun my/uuidgen-4 (arg)
  "Return an UUID from random numbers (UUIDv4).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-4)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))

(defun my/avy-goto-url()
  "Use avy to go to an URL in the buffer."
  (interactive)
  (avy--generic-jump "https?://" nil 'pre))
(defun my/avy-open-url ()
  "Use avy to select an URL in the buffer and open it."
  (interactive)
  (save-excursion
    (my/avy-goto-url)
    (browse-url-at-point)))

(defun my/smartparens-pair-newline (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun my/smartparens-pair-newline-and-indent (id action context)
  (my/smartparens-pair-newline id action context)
  (indent-according-to-mode))

(defun my/smart-closing-parenthesis ()
  (interactive)
  (let* ((sp-navigate-close-if-unbalanced t)
         (current-pos (point))
         (current-line (line-number-at-pos current-pos))
         (next-pos (save-excursion
                     (sp-up-sexp)
                     (point)))
         (next-line (line-number-at-pos next-pos)))
    (cond
     ((and (= current-line next-line)
           (not (= current-pos next-pos)))
      (sp-up-sexp))
     (t
      (insert-char ?\))))))

(provide 'my//basic/editing/funcs)
