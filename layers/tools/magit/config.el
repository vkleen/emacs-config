(require 'my//tools/magit/funcs "funcs")

(evil-define-key 'motion magit-mode-map
  (kbd dotfile-leader-key) my-default-map)
(setq git-link-open-in-browser t)
(my/declare-prefix "gl" "links")
(my/set-leader-keys
  "gI" 'helm-gitignore
  "gll" 'my/git-link
  "glL" 'my/git-link-copy-url-only
  "glc" 'my/git-link-commit
  "glC" 'my/git-link-commit-copy-url-only

  "gM" 'git-messenger:popup-message)
(define-key git-messenger-map [escape] 'git-messenger:popup-close)

(my/set-leader-keys
 "gt" 'my/time-machine-transient-state/body)
(my|define-transient-state time-machine
  :title "Git Timemachine Transient State"
  :doc "
[_p_/_N_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit"
  :on-enter (let (golden-ratio-mode)
              (unless (bound-and-true-p git-timemachine-mode)
                (call-interactively 'git-timemachine)))
  :on-exit (when (bound-and-true-p git-timemachine-mode)
             (git-timemachine-quit))
  :foreign-keys run
  :bindings
  ("c" git-timemachine-show-current-revision)
  ("g" git-timemachine-show-nth-revision)
  ("p" git-timemachine-show-previous-revision)
  ("n" git-timemachine-show-next-revision)
  ("N" git-timemachine-show-previous-revision)
  ("Y" git-timemachine-kill-revision)
  ("q" nil :exit t))

(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
(add-hook 'git-commit-mode-hook 'fci-mode)

(my/declare-prefix "gd" "diff")
(my/declare-prefix "gf" "file")
(my/set-leader-keys
 "gb"  'my/git-blame-transient-state/body
 "gfh" 'magit-log-buffer-file
 "gm"  'magit-dispatch-popup
 "gs"  'magit-status
 "gS"  'magit-stage-file
 "gU"  'magit-unstage-file)
(my|define-transient-state git-blame
  :title "Git Blame Transient State"
  :doc "
Press [_b_] again to blame further in the history, [_q_] to go up or quit."
  :on-enter (let (golden-ratio-mode)
              (unless (bound-and-true-p magit-blame-mode)
                (call-interactively 'magit-blame)))
  :foreign-keys run
  :bindings
  ("b" magit-blame)
  ;; here we use the :exit keyword because we should exit the
  ;; micro-state only if the magit-blame-quit effectively disable
  ;; the magit-blame mode.
  ("q" nil :exit (progn (when (bound-and-true-p magit-blame-mode)
                          (magit-blame-quit))
                        (not (bound-and-true-p magit-blame-mode)))))

(add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
(define-key magit-status-mode-map (kbd "C-S-w") 'my/magit-toggle-whitespace)
(setq magit-display-buffer-function 'my//fullscreen-magit)

(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
(define-key magit-mode-map "%" 'magit-gitflow-popup)
(my|diminish magit-gitflow-mode "Flow")

(my/declare-prefix "gH" "highlight")
(let ((descr
       '(("smeargle" . "highlight by last update time")
         ("smeargle-commits" . "highlight by age of changes")
         ("smeargle-clear" . "clear"))))
  (--each descr
    (push (cons (concat "\\`" (car it) "\\'") (cdr it))
          which-key-description-replacement-alist)))
(my/set-leader-keys
  "gHc" 'smeargle-clear
  "gHh" 'smeargle-commits
  "gHt" 'smeargle)
