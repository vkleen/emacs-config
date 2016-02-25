(require 'notmuch)

(defun my-forward (prefix)
  (interactive "P")
  (let ((message-forward-as-mime nil)
        (message-forward-ignored-headers ".*"))
    (notmuch-show-forward-message prefix)))

(define-key notmuch-show-mode-map "F" 'my-forward)
(define-key notmuch-tree-mode-map "F" 'my-forward)
(define-key notmuch-search-mode-map "F" 'my-forward)

(defmacro make-tag-key-binding (doc key tags)
  `(progn
     (define-key notmuch-show-mode-map ,key
       (lambda ()
         ,doc
         (interactive)
         (notmuch-show-tag ,tags)))
     (define-key notmuch-tree-mode-map ,key
       (lambda ()
         ,doc
         (interactive)
         (notmuch-tree-tag ,tags)))
     (define-key notmuch-search-mode-map ,key
       (lambda (&optional beg end)
         ,doc
         (interactive (notmuch-search-interactive-region))
         (notmuch-search-tag ,tags beg end)))))

(setq notmuch-tag-macro-alist
  (list
   '("tag spam" "S" '("+spam" "+deleted" "-inbox" "-unread"))))

(dolist (b notmuch-tag-macro-alist)
  (eval `(make-tag-key-binding . ,b)))

(require 'notmuch-address)
(setq notmuch-address-command "/home/vkleen/bin/notmuch_addresses.py")
(notmuch-address-message-insinuate)
(setq notmuch-address-selection-function
      (lambda (prompt collection initial-input)
        (completing-read prompt (cons initial-input collection) nil t nil 'notmuch-address-history)))

(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

(defvar notmuch-hello-refresh-count 0)

(defun notmuch-hello-refresh-status-message ()
  (unless no-display
    (let* ((new-count
            (string-to-number
             (car (process-lines notmuch-command "count"))))
           (diff-count (- new-count notmuch-hello-refresh-count)))
      (cond
       ((= notmuch-hello-refresh-count 0)
        (message "You have %s messages."
                 (notmuch-hello-nice-number new-count)))
       ((> diff-count 0)
        (message "You have %s more messages since last refresh."
                 (notmuch-hello-nice-number diff-count)))
       ((< diff-count 0)
        (message "You have %s fewer messages since last refresh."
                 (notmuch-hello-nice-number (- diff-count)))))
      (setq notmuch-hello-refresh-count new-count))))

(add-hook 'notmuch-hello-refresh-hook 'notmuch-hello-refresh-status-message)

(custom-set-variables
 '(notmuch-crypto-process-mime t)
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i" :search-type tree)
     (:name "unread" :query "tag:unread" :key "u" :search-type tree)
     (:name "flagged" :query "tag:flagged" :key "f" :search-type tree)
     (:name "TODO" :query "tag:todo" :key "t" :search-type tree)
     (:name "Plans" :query "tag:plans" :key "p" :search-type tree))))
 '(send-mail-function (quote sendmail-send-it)))
