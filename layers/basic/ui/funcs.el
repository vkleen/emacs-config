(defun my/set-default-font (plists)
  "Set the font given the passed PLISTS.

PLISTS has either the form (\"fontname\" :prop1 val1 :prop2 val2 ...)
or is a list of such. The first font that can be found will
be used.

The return value is nil if no font was found, truthy otherwise."
  (unless (listp (car plists))
    (setq plists (list plists)))
  (catch 'break
    (dolist (plist plists)
      (when (find-font (font-spec :name (car plist)))
        (let* ((font (car plist))
               (props (cdr plist))
               (fontspec (apply 'font-spec :name font props)))
          (my/message "Setting font \"%s\"..." font)
          (set-frame-font fontspec nil t)
          (push `(font . ,(frame-parameter nil 'font)) default-frame-alist))
        (throw 'break t)))
    nil))

(defun my/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(defun my/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (kill-buffer (current-buffer))))

(defun my/alternate-buffer ()
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (if (evil-alternate-buffer)
      (switch-to-buffer (car (evil-alternate-buffer)))
    (switch-to-buffer (other-buffer (current-buffer) t))))

(defun my/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

(defun my/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? " (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (message "Buffers deleted!")))

;; from https://github.com/gempesaw/dotemacs/blob/emacs/dg-defun.el
(defun my/kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP. This
function, unlike the built-in `kill-matching-buffers` does so
WITHOUT ASKING. The optional second argument indicates whether to
kill internal buffers too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

(defun my/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defvar my-useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful.")
(defvar my-useful-buffers-regexp '("\\*scratch\\*")
  "Regexp used to define buffers that are useful despite matching
`my-useless-buffers-regexp'.")

(defun my/useless-buffer-p (buffer)
  "Determines if a buffer is useful."
  (let ((buf-paren-major-mode (get (with-current-buffer buffer
                                     major-mode)
                                   'derived-mode-parent))
        (buf-name (buffer-name buffer)))
    ;; first find if useful buffer exists, if so returns nil and don't check for
    ;; useless buffers. If no useful buffer is found, check for useless buffers.
    (unless (cl-loop for regexp in my-useful-buffers-regexp do
                     (when (or (eq buf-paren-major-mode 'comint-mode)
                               (string-match regexp buf-name))
                       (cl-return t)))
      (cl-loop for regexp in my-useless-buffers-regexp do
               (when (string-match regexp buf-name)
                 (cl-return t))))))

(defun my/next-useful-buffer ()
  "Switch to the next buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (next-buffer)
    (while (and (my/useless-buffer-p (current-buffer))
                (not (eq (current-buffer) start-buffer)))
      (next-buffer))))

(defun my/previous-useful-buffer ()
  "Switch to the previous buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (previous-buffer)
    (while (and (my/useless-buffer-p (current-buffer))
                (not (eq (current-buffer) start-buffer)))
      (previous-buffer))))

(defun my/new-empty-buffer ()
  "Create a new buffer Called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun my/safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

(defvar dotfile-scratch-mode 'text-mode
  "Default major mode of the scratch buffer.")

(defun my/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer. Create it first if needed."
  (interactive)
  (let ((exists (get-buffer "*scratch*")))
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (when (and (not exists)
               (not (eq major-mode dotfile-scratch-mode))
               (fboundp dotfile-scratch-mode))
      (funcall dotfile-scratch-mode))))

(defun my/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun my/next-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (if (and (boundp 'flycheck-mode)
           (symbol-value flycheck-mode)
           (not (get-buffer-window "*compilation*")))
      (call-interactively 'flycheck-next-error)
    (call-interactively 'next-error)))

(defun my/previous-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (if (and (boundp 'flycheck-mode)
           (symbol-value flycheck-mode)
           (not (get-buffer-window "*compilation*")))
      (call-interactively 'flycheck-previous-error)
    (call-interactively 'previous-error)))

(defun my/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

(defun my/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (and (fboundp 'projectile-project-p)
                   (projectile-project-p))
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

(defun my/find-user-init-file ()
  "Edit the `user-init-file', in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun my/find-dotfile ()
  "Edit the `dotfile', in the current window."
  (interactive)
  (find-file-existing dotfile-filepath))

(defun my/sudo-edit (&optional arg)
  (interactive "p")
  (let ((fname (if (or arg (not buffer-file-name)) (read-file-name "File: ") buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
              (insert fname)
              (search-backward ":")
              (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname) last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
              (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))

(defun my/open-in-external-app ()
  "Open current file in external application."
  (interactive)
  (let ((file-path (if (eq major-mode 'dired-mode)
                       (dired-get-file-for-visit)
                     (buffer-file-name))))
    (if file-path
        (let ((process-connection-type nil))
          (start-process "" nil "xdg-open" file-path))
      (message "No file associated to this buffer."))))

(defun my/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
               (when (and (fbounp 'projectile-project-p)
                          (projectile-project-p))
                 (call-interactively #'projectile-invalidate-cache))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun my/show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun my/insert-line-above-no-indent (count)
  (interactive "p")
  (let ((p (+ (point) count)))
    (save-excursion
       (if (eq (line-number-at-pos) 1)
          (evil-move-beginning-of-line)
        (progn
          (evil-previous-line)
          (evil-move-end-of-line)))
      (while (> count 0)
        (insert "\n")
        (setq count (1- count))))
    (goto-char p)))

(defun my/insert-line-below-no-indent (count)
  "Insert a new line below with no identation."
  (interactive "p")
  (save-excursion
    (evil-move-end-of-line)
    (while (> count 0)
      (insert "\n")
      (setq count (1- count)))))

;; insert one or several line below without changing current evil state
(defun my/evil-insert-line-below (count)
  "Insert one of several lines below the current point's line without changing
the current state and point position."
  (interactive "p")
  (save-excursion
    (evil-save-state (evil-open-below count))))

;; insert one or several line above without changing current evil state
(defun my/evil-insert-line-above (count)
  "Insert one of several lines above the current point's line without changing
the current state and point position."
  (interactive "p")
  (save-excursion
    (evil-save-state (evil-open-above count))))

(defvar my-indent-sensitive-modes
  '(coffee-mode
    elm-mode
    haml-mode
    haskell-mode
    slim-mode
    makefile-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    python-mode
    yaml-mode)
  "Modes for which auto-indenting is suppressed.")

(defun my/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode my-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (evil-indent (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(defun my/evil-goto-next-line-and-indent (&optional count)
  (interactive "p")
  (let ((counter (or count 1)))
    (while (> counter 0)
      (join-line 1)
      (newline-and-indent)
      (setq counter (1- counter)))))

(defun my/push-mark-and-goto-beginning-of-line ()
  "Push a mark at current location and go to the beginnign of the line."
  (interactive)
  (push-mark (point))
  (evil-beginning-of-line))

(defun my/push-mark-and-goto-end-of-line ()
  "Push a mark at current location and go to the end of the line."
  (interactive)
  (push-mark (point))
  (evil-end-of-line))

(defun my/jump-in-buffer ()
  (interactive)
  (call-interactively
   (if (eq major-mode 'org-mode)
       'helm-org-in-buffer-headings
     'helm-semantic-or-imenu)))

(defun my/close-compilation-window ()
  "Close the window containing the '*compilation*' buffer."
  (interactive)
  (when compilation-last-buffer
    (delete-windows-on compilation-last-buffer)))

(defvar dotfile-persistent-server nil
  "If non nil advises quit functions to keep server open when quitting.")

(defvar my-really-kill-emacs nil
  "prevent window manager close from closing instance.")

(defadvice save-buffers-kill-emacs (around my-really-exit activate)
  "Only kill emacs if a prefix is set"
  (if (or my-really-kill-emacs (not dotfile-persistent-server))
      ad-do-it
    (my/frame-killer)))

(defun my/save-buffers-kill-emacs ()
  "Save all changed buffers and exit Spacemacs"
  (interactive)
  (setq my-really-kill-emacs t)
  (save-buffers-kill-emacs))

(defun my/kill-emacs ()
  "Lose all changes and exit Spacemacs"
  (interactive)
  (setq my-really-kill-emacs t)
  (kill-emacs))

(defun my/prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Spacemacs"
  (interactive)
  (setq my-really-kill-emacs t)
  (save-some-buffers)
  (kill-emacs))

(defun my/frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (condition-case-unless-debug nil
      (delete-frame nil 1)
      (error
       (make-frame-invisible nil 1))))

(defun my/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun my/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun my/layout-triple-columns ()
  "Set the layout to triple columns. "
  (interactive)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

(defun my/layout-double-columns ()
  "Set the layout to double columns. "
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun my/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun my/toggle-current-window-dedication ()
  "Toggle dedication state of a window."
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

;; A small minor mode to use a big fringe adapted from
;; http://bzg.fr/emacs-strip-tease.html
(define-minor-mode my-centered-buffer-mode
  "Minor mode to use big fringe in the current buffer."
  :global t
  :init-value nil
  :group 'editing-basics
  (if my-centered-buffer-mode
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)
        (set-fringe-mode
         (/ (- (frame-pixel-width)
               (* 100 (frame-char-width)))
            2)))
    (set-fringe-style nil)
    (when (assoc ?_ register-alist)
      (jump-to-register ?_))))

;; from magnars modified by ffevotte for dedicated windows support
(defun my/rotate-windows (count)
  "Rotate your windows.
Dedicated windows are left untouched. Giving a negative prefix
argument makes the windows rotate backwards."
  (interactive "p")
  (let* ((non-dedicated-windows (remove-if 'window-dedicated-p (window-list)))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))

                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))

(defun my/rotate-windows-backward (count)
  "Rotate your windows backward."
  (interactive "p")
  (my/rotate-windows (* -1 count)))

;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun my/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let ((complete-regexp (if after
                             (concat regexp "\\([ \t]*\\)")
                           (concat "\\([ \t]*\\)" regexp)))
        (group (if justify-right -1 1)))
    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun my/align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro my|create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "my/align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (my/align-repeat start end ,regexp ,justify-right after)))))

(my|create-align-repeat-x "comma" "," nil t)
(my|create-align-repeat-x "semicolon" ";" nil t)
(my|create-align-repeat-x "colon" ":" nil t)
(my|create-align-repeat-x "equal" "=")
(my|create-align-repeat-x "math-oper" "[+\\-*/]")
(my|create-align-repeat-x "ampersand" "&")
(my|create-align-repeat-x "bar" "|")
(my|create-align-repeat-x "left-paren" "(")
(my|create-align-repeat-x "right-paren" ")" t)
(my|create-align-repeat-x "backslash" "\\\\")

(defun my/uniquify-lines ()
  "Remove duplicate adjacent lines in region or current buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (let ((beg (if (region-active-p) (region-beginning) (point-min)))
            (end (if (region-active-p) (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

(defun my/sort-lines ()
  "Sort lines in region or current buffer"
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (sort-lines nil beg end)))

(defun my/count-words-analysis (start end)
  "Count how many times each word is used in the region.
 Punctuation is ignored."
  (interactive "r")
  (let (words alist_words_compare (formated ""))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (defun alist_words_compare (a b)
      "Compare elements from an associative list of words count.
Compare them on count first,and in case of tie sort them alphabetically."
      (let ((a_key (car a))
            (a_val (cdr a))
            (b_key (car b))
            (b_val (cdr b)))
        (if (eq a_val b_val)
            (string-lessp a_key b_key)
          (> a_val b_val))))
    (setq words (cl-sort words 'alist_words_compare))
    (while words
      (let* ((word (pop words))
             (name (car word))
             (count (cdr word)))
        (setq formated (concat formated (format "[%s: %d], " name count)))))
    (when (interactive-p)
      (if (> (length formated) 2)
          (message (substring formated 0 -2))
        (message "No words.")))
    words))

(defun my/shrink-window-horizontally (delta)
  "Wrap `my/shrink-window-horizontally'."
  (interactive "p")
  (shrink-window delta t))

(defun my/shrink-window (delta)
  "Wrap `my/shrink-window'."
  (interactive "p")
  (shrink-window delta))

(defun my/enlarge-window (delta)
  "Wrap `my/enlarge-window'."
  (interactive "p")
  (enlarge-window delta))

(defun my/enlarge-window-horizontally (delta)
  "Wrap `my/enlarge-window-horizontally'."
  (interactive "p")
  (enlarge-window delta t))

(defun my/scale-up-or-down-font-size (direction)
  "Scale the font. If DIRECTION is positive or zero the font is scaled up,
otherwise it is scaled down."
  (interactive)
  (let ((scale 0.5))
    (if (eq direction 0)
        (text-scale-set 0)
      (if (< direction 0)
          (text-scale-decrease scale)
        (text-scale-increase scale)))))

(defun my/scale-up-font ()
  "Scale up the font."
  (interactive)
  (my/scale-up-or-down-font-size 1))

(defun my/scale-down-font ()
  "Scale up the font."
  (interactive)
  (my/scale-up-or-down-font-size -1))

(defun my/reset-font-size ()
  "Reset the font size."
  (interactive)
  (my/scale-up-or-down-font-size 0))

(defun my/anzu-update-mode-line (here total)
  "Custom update function which does not propertize the status."
  (when anzu--state
    (let ((status (cl-case anzu--state
                    (search (format "(%s/%d%s)"
                                    (anzu--format-here-position here total)
                                    total (if anzu--overflow-p "+" "")))
                    (replace-query (format "(%d replace)" total))
                    (replace (format "(%d/%d)" here total)))))
      status)))

(defun my/comment-or-uncomment-lines-inverse (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-comment-or-uncomment-lines arg)))

(defun my/comment-or-uncomment-lines (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-comment-or-uncomment-lines arg)))

(defun my/copy-and-comment-lines-inverse (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-copy-and-comment-lines arg)))

(defun my/copy-and-comment-lines (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-copy-and-comment-lines arg)))

(defun my/quick-comment-or-uncomment-to-the-line-inverse
    (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-comment-or-uncomment-to-the-line arg)))

(defun my/quick-comment-or-uncomment-to-the-line (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-comment-or-uncomment-to-the-line arg)))

(defun my/comment-or-uncomment-paragraphs-inverse (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-comment-or-uncomment-paragraphs arg)))

(defun my/comment-or-uncomment-paragraphs (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-comment-or-uncomment-paragraphs arg)))

(defun my/evil-search-clear-highlight ()
  "Clear evil-search or evil-ex-search persistent highlights."
  (interactive)
  (case evil-search-module
    ('isearch (evil-search-highlight-persist-remove-all))
    ('evil-search (evil-ex-nohighlight))))

(defun my/adaptive-evil-highlight-persist-face ()
  (set-face-attribute 'evil-search-highlight-persist-highlight-face nil
                      :inherit 'region
                      :background nil
                      :foreground nil))

(defun my/disable-vi-tilde-fringe ()
  "Disable `vi-tilde-fringe' in the current buffer."
  (vi-tilde-fringe-mode -1))

(defun my/disable-vi-tilde-fringe-read-only ()
  "Disable `vi-tilde-fringe' in the current buffer if it is read only."
  (when buffer-read-only
    (my/disable-vi-tilde-fringe)))

(defun my/open-junk-file (&optional arg)
  "Open junk file Open junk file using helm or ivy depending
on whether the `ivy' layer is used or not, with
`prefix-arg' search in junk files"
  (interactive "P")
  (let* ((fname (format-time-string open-junk-file-format (current-time)))
         (rel-fname (file-name-nondirectory fname))
         (junk-dir (file-name-directory fname))
         (default-directory junk-dir))
    (let (helm-ff-newfile-prompt-p)
    (if arg
        (my/helm-files-smart-do-search)
      (helm-find-files-1 fname)))))

(defun my/restart-emacs (&optional args)
  "Restart emacs."
  (interactive)
  (setq my-really-kill-emacs t)
  (restart-emacs args))
(defun my/restart-emacs-resume-layouts (&optional args)
  "Restart emacs and resume layouts."
  (interactive)
  (my/restart-emacs (cons "--resume-layouts" args)))
(defun my/restart-emacs-debug-init (&optional args)
  "Restart emacs and enable debug-init."
  (interactive)
  (my/restart-emacs (cons "--debug-init" args)))

(defun my/no-golden-ratio-for-buffers (bufname)
  "Disable golden-ratio if BUFNAME is the name of a visible buffer."
  (and (get-buffer bufname) (get-buffer-window bufname 'visible)))

(defun my/no-golden-ratio-guide-key ()
  "Disable golden-ratio for guide-key popwin buffer."
  (or (my/no-golden-ratio-for-buffers " *guide-key*")
      (my/no-golden-ratio-for-buffers " *popwin-dummy*")))

(defun my/neotree-expand-or-open ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when neo-auto-indent-point
              (next-line)
              (neo-point-auto-indent)))
        (call-interactively 'neotree-enter)))))

(defun my/neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when neo-auto-indent-point
        (neo-point-auto-indent)))))

(defun my/neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (my/neotree-collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))

(defun neotree-find-project-root ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((origin-buffer-file-name (buffer-file-name)))
      (neotree-find (projectile-project-root))
      (neotree-find origin-buffer-file-name))))

(defun my//neotree-maybe-attach-window ()
  (when (get-buffer-window (neo-global--get-buffer))
    (neo-global--attach)))

(defun my/enable-smooth-scrolling ()
  "Enable smooth scrolling."
  (interactive)
  (setq scroll-conservatively 101))

(defun my/disable-smooth-scrolling ()
  "Enable smooth scrolling."
  (interactive)
  (setq scroll-conservatively 0))


(defun evil-unimpaired//find-relative-filename (offset)
  (when buffer-file-name
    (let* ((directory (f-dirname buffer-file-name))
           (files (f--files directory (not (s-matches? "^\\.?#" it))))
           (index (+ (-elem-index buffer-file-name files) offset))
           (file (and (>= index 0) (nth index files))))
      (when file
        (f-expand file directory)))))

(defun evil-unimpaired/previous-file ()
  (interactive)
  (-if-let (filename (evil-unimpaired//find-relative-filename -1))
      (find-file filename)
    (user-error "No previous file")))

(defun evil-unimpaired/next-file ()
  (interactive)
  (-if-let (filename (evil-unimpaired//find-relative-filename 1))
      (find-file filename)
    (user-error "No next file")))

(defun evil-unimpaired/paste-above ()
  (interactive)
  (evil-insert-newline-above)
  (evil-paste-after 1))

(defun evil-unimpaired/paste-below ()
  (interactive)
  (evil-insert-newline-below)
  (evil-paste-after 1))

(defun evil-unimpaired/insert-space-above ()
  (interactive)
  (save-excursion
    (evil-insert-newline-above)))

(defun evil-unimpaired/insert-space-below ()
  (interactive)
  (save-excursion
    (evil-insert-newline-below)))

(defun evil-unimpaired/next-frame ()
  (interactive)
  (raise-frame (next-frame)))

(defun evil-unimpaired/previous-frame ()
  (interactive)
  (raise-frame (previous-frame)))

(defun my//zoom-frm-powerline-reset ()
  (when (fboundp 'powerline-reset)
    (setq-default powerline-height (frame-char-height))
    (powerline-reset)))

(defun my//zoom-frm-do (arg)
  "Perform a zoom action depending on ARG value."
  (let ((zoom-action (cond ((eq arg 0) 'zoom-frm-unzoom)
                           ((< arg 0) 'zoom-frm-out)
                           ((> arg 0) 'zoom-frm-in)))
        (fm (cdr (assoc 'fullscreen (frame-parameters))))
        (fwp (* (frame-char-width) (frame-width)))
        (fhp (* (frame-char-height) (frame-height))))
    (when (equal fm 'maximized)
      (toggle-frame-maximized))
    (funcall zoom-action)
    (set-frame-size nil fwp fhp t)
    (when (equal fm 'maximized)
      (toggle-frame-maximized))))

(defun my/zoom-frm-in ()
  "zoom in frame, but keep the same pixel size"
  (interactive)
  (my//zoom-frm-do 1)
  (my//zoom-frm-powerline-reset))

(defun my/zoom-frm-out ()
  "zoom out frame, but keep the same pixel size"
  (interactive)
  (my//zoom-frm-do -1)
  (my//zoom-frm-powerline-reset))

(defun my/zoom-frm-unzoom ()
  "Unzoom current frame, keeping the same pixel size"
  (interactive)
  (my//zoom-frm-do 0)
  (my//zoom-frm-powerline-reset))

(defvar-local my-last-ahs-highlight-p nil
  "Info on the last searched and highlighted symbol.")
(defvar-local my--ahs-searching-forward t)

(defun my/goto-last-searched-ahs-symbol ()
  "Go to the last known occurrence of the last symbol searched with
`auto-highlight-symbol'."
  (interactive)
  (if my-last-ahs-highlight-p
      (progn (goto-char (nth 1 my-last-ahs-highlight-p))
             (my/ahs-highlight-now-wrapper)
             (my/symbol-highlight-transient-state/body))
    (message "No symbol has been searched for now.")))

(defun my/integrate-evil-search (forward)
  ;; isearch-string is last searched item.  Next time
  ;; "n" is hit we will use this.
  (setq isearch-string
        (concat "\\<" (evil-find-thing forward 'symbol) "\\>")
        isearch-regexp
        (concat "\\<" (evil-find-thing forward 'symbol) "\\>"))
  ;; Next time "n" is hit, go the correct direction.
  (setq isearch-forward forward)
  ;; ahs does a case sensitive search.  We could set
  ;; this, but it would break the user's current
  ;; sensitivity settings.  We could save the setting,
  ;; then next time the user starts a search we could
  ;; restore the setting.
  ;;(setq case-fold-search nil)
  ;; Place the search term into the search rings.
  (isearch-update-ring isearch-string t)
  (evil-push-search-history isearch-string forward)
  ;; Use this search term for empty pattern "%s//replacement/"
  ;; Append case sensitivity
  (setq evil-ex-last-was-search nil
        evil-ex-substitute-pattern `(,(concat isearch-string "\\C")
                                     nil (0 0))))

(defun my/ensure-ahs-enabled-locally ()
  "Ensures ahs is enabled for the local buffer."
  (unless
      (bound-and-true-p ahs-mode-line)
    (auto-highlight-symbol-mode)
    ))

(defun my/ahs-highlight-now-wrapper ()
  "Safe wrapper for ahs-highlight-now"
  (eval '(progn
           (my/ensure-ahs-enabled-locally)
           (ahs-highlight-now)) nil))

(defun my/enter-ahs-forward ()
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (setq my--ahs-searching-forward t)
  (my/quick-ahs-forward))

(defun my/enter-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (setq my--ahs-searching-forward nil)
  (my/quick-ahs-forward))

(defun my/quick-ahs-forward ()
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (my//quick-ahs-move t))

(defun my/quick-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
  (interactive)
  (my//quick-ahs-move nil))

(defun my//quick-ahs-move (forward)
  "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"

  (if (eq forward my--ahs-searching-forward)
      (progn
        (my/integrate-evil-search t)
        (my/ahs-highlight-now-wrapper)
        (evil-set-jump)
        (my/symbol-highlight-transient-state/body)
        (ahs-forward))
    (progn
      (my/integrate-evil-search nil)
      (my/ahs-highlight-now-wrapper)
      (evil-set-jump)
      (my/symbol-highlight-transient-state/body)
      (ahs-backward))))

(defun my/symbol-highlight ()
  "Highlight the symbol under point with `auto-highlight-symbol'."
  (interactive)
  (my/ahs-highlight-now-wrapper)
  (setq my-last-ahs-highlight-p (ahs-highlight-p))
  (my/symbol-highlight-transient-state/body)
  (my/integrate-evil-search nil))

(defun my//ahs-ms-on-exit ()
  ;; Restore user search direction state as ahs has exitted in a state
  ;; good for <C-s>, but not for 'n' and 'N'"
  (setq isearch-forward my--ahs-searching-forward))

(defun my/symbol-highlight-reset-range ()
  "Reset the range for `auto-highlight-symbol'."
  (interactive)
  (ahs-change-range ahs-default-range))

      (defun symbol-highlight-doc ()
        (let* ((i 0)
               (overlay-count (length ahs-overlay-list))
               (overlay (format "%s" (nth i ahs-overlay-list)))
               (current-overlay (format "%s" ahs-current-overlay))
               (st (ahs-stat))
               (plighter (ahs-current-plugin-prop 'lighter))
               (plugin (format "%s"
                               (cond ((string= plighter "HS")  "Display")
                                     ((string= plighter "HSA") "Buffer")
                                     ((string= plighter "HSD") "Function"))))
               (face (cond ((string= plighter "HS")  ahs-plugin-defalt-face)
                           ((string= plighter "HSA") ahs-plugin-whole-buffer-face)
                           ((string= plighter "HSD") ahs-plugin-bod-face))))
          (while (not (string= overlay current-overlay))
            (setq i (1+ i))
            (setq overlay (format "%s" (nth i ahs-overlay-list))))
          (let* ((x/y (format "[%s/%s]" (- overlay-count i) overlay-count))
                 (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" "")))
            (concat
             (propertize (format " %s " plugin) 'face face)
             (propertize (format " %s%s " x/y hidden) 'face
                         `(:foreground "#ffffff" :background "#000000"))))))

(defun ahs-to-iedit ()
  (interactive)
  (evil-iedit-state/iedit-mode)
  (iedit-restrict-region (ahs-current-plugin-prop 'start)
                         (ahs-current-plugin-prop 'end)))
;; transient state
(defun my//symbol-highlight-ts-doc ()
  (my//transient-state-make-doc
   'symbol-highlight
   (format my--symbol-highlight-transient-state-doc
           (symbol-highlight-doc)
           (make-string (length (symbol-highlight-doc)) 32))))

(defun my/backward-capitalize-word ()
  (interactive)
  (capitalize-word -1))

(provide 'my//basic/ui/funcs)
