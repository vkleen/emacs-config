(require 'notmuch)

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
