(defvar dotfile-search-tools '("ag" "grep")
  "List of search tool executable names. Spacemacs uses the first installed
tool of the list. Supported tools are `ag' and `grep'.")

(defvar dotfile-helm-no-header nil
  "if non nil, the helm header is hidden when there is only one source.")

(defun my//hide-cursor-in-helm-buffer ()
  "Hide the cursor in helm buffers."
  (with-helm-buffer
    (setq cursor-in-non-selected-windows nil)))

(defun my//set-dotted-directory ()
  "Set the face of diretories for `.' and `..'"
  (set-face-attribute 'helm-ff-dotted-directory
                      nil
                      :foreground nil
                      :background nil
                      :inherit 'helm-ff-directory))

(defvar my-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
(defvar my-helm-display-buffer-regexp
  `("*.*helm.**"
    (display-buffer-in-side-window)
    (inhibit-same-window . t)
    (side . right)
    (window-width . 0.6)
    (window-height . 0.4)))
(defvar my-display-buffer-alist nil)

(defun my//helm-prepare-display ()
  "Prepare necessary settings to make Helm display properly."
  ;; avoid Helm buffer being diplaye twice when user
  ;; sets this variable to some function that pop buffer to
  ;; a window. See https://github.com/syl20bnr/spacemacs/issues/1396
  (let ((display-buffer-base-action '(nil)))
    (setq my-display-buffer-alist display-buffer-alist)
    ;; the only buffer to display is Helm, nothing else we must set this
    ;; otherwise Helm cannot reuse its own windows for copyinng/deleting
    ;; etc... because of existing popwin buffers in the alist
    (setq display-buffer-alist nil)
    (popwin-mode -1)
    ;; workaround for a helm-evil incompatibility
    ;; see https://github.com/syl20bnr/spacemacs/issues/3700
    (when helm-prevent-escaping-from-minibuffer
      (define-key evil-motion-state-map [down-mouse-1] nil))))

(defun my//display-helm-window (buffer)
  "Display the Helm window."
  (let ((display-buffer-alist
         (list my-helm-display-help-buffer-regexp
               ;; this or any specialized case of Helm buffer must be
               ;; added AFTER `my-helm-display-buffer-regexp'.
               ;; Otherwise, `my-helm-display-buffer-regexp' will
               ;; be used before
               ;; `my-helm-display-help-buffer-regexp' and display
               ;; configuration for normal Helm buffer is applied for helm
               ;; help buffer, making the help buffer unable to be
               ;; displayed.
               my-helm-display-buffer-regexp)))
    (helm-default-display-buffer buffer)))

(defun my//restore-previous-display-config ()
  "Workaround for a helm-evil incompatibility
 see https://github.com/syl20bnr/spacemacs/issues/3700"
  (when helm-prevent-escaping-from-minibuffer
    (define-key evil-motion-state-map
      [down-mouse-1] 'evil-mouse-drag-region))
  (popwin-mode 1)
  ;; we must enable popwin-mode first then restore `display-buffer-alist'
  ;; Otherwise, popwin keeps adding up its own buffers to
  ;; `display-buffer-alist' and could slow down Emacs as the list grows
  (setq display-buffer-alist my-display-buffer-alist))

(defun my//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(defun my//helm-hjkl-navigation ()
  "Set navigation on 'hjkl' for the given editing STYLE."
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-h") 'helm-next-source)
  (define-key helm-map (kbd "C-S-h") 'describe-key)
  (define-key helm-map (kbd "C-l") (kbd "RET"))
  (with-eval-after-load 'helm-files
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
      (define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
      ;; rebind `describe-key' for convenience
      (define-key keymap (kbd "C-S-h") 'describe-key))))

(defun my//helm-make-source (f &rest args)
  "Function to be used as advice to activate fuzzy matching for all sources."
  (let ((source-type (cadr args))
        (props (cddr args)))
    (unless (eq source-type 'helm-source-async)
      (plist-put props :fuzzy-match t)))
  (apply f args))

(defun my//define-helm-action-functions ()
  "Define Spacemacs functions to pick actions."
  (dotimes (n 10)
    (let ((func (intern (format "my/helm-action-%d" n)))
          (doc (format "Select helm action #%d" n)))
      (eval `(defun ,func ()
               ,doc
               (intern)
               (helm-select-nth-action ,(1- n)))))))

(defun my/helm-ts-edit ()
  "Switch in edit mode depending on the current helm buffer."
  (interactive)
  (cond
   ((string-equal "*helm-ag*" helm-buffer)
    (helm-ag-edit))))

(defun my//helm-navigation-ts-on-enter ()
  "Initialization of helm transient-state."
  ;; faces
  (my//helm-navigation-ts-set-face)
  (setq my--helm-navigation-ts-face-cookie-minibuffer
        (face-remap-add-relative
         'minibuffer-prompt
         'my-helm-navigation-ts-face)))

(defun my//helm-navigation-ts-set-face ()
  "Set the face for helm header in helm navigation transient-state"
  (with-helm-window
    (setq my--helm-navigation-ts-face-cookie-header
          (face-remap-add-relative
           'helm-header
           'my-helm-navigation-ts-face))))

(defun my//helm-navigation-ts-on-exit ()
  "Action to perform when exiting helm transient-state."
  (with-helm-window
    (face-remap-remove-relative
     my--helm-navigation-ts-face-cookie-header))
  (face-remap-remove-relative
   my--helm-navigation-ts-face-cookie-minibuffer))

(defun my/helm-transient-state-select-action ()
  "Display the Helm actions page."
  (interactive)
  (call-interactively 'helm-select-action)
  (my//helm-navigation-ts-set-face))

(defun my/helm-toggle-header-line ()
  "Hide the `helm' header if there is only one source."
  (when dotfile-helm-no-header
    (if (> (length helm-sources) 1)
        (set-face-attribute
         'helm-source-header
         nil
         :foreground helm-source-header-default-foreground
         :background helm-source-header-default-background
         :box helm-source-header-default-box
         :height helm-source-header-default-height)
      (set-face-attribute
       'helm-source-header
       nil
       :foreground (face-attribute 'default :background)
       :background (face-attribute 'default :background)
       :box nil
       :height 0.1))))

(defun my//helm-cleanup ()
  "Cleanup some helm related states when quitting."
  ;; deactivate any running transient map (transient-state)
  (setq overriding-terminal-local-map nil))

(defun my/helm-find-files (arg)
  "Custom spacemacs implementation for calling helm-find-files-1.
Removes the automatic guessing of the initial value based on thing at point. "
  (interactive "P")
  (let* ((hist (and arg helm-ff-history (helm-find-files-history)))
         (default-input hist)
         (input (cond ((and (eq major-mode 'dired-mode) default-input)
                       (file-name-directory default-input))
                      ((and (not (string= default-input ""))
                            default-input))
                      (t (expand-file-name (helm-current-directory))))))
    (set-text-properties 0 (length input) nil input)
    (helm-find-files-1 input)))

(defun my//helm-find-files-edit (candidate)
  "Opens a dired buffer and immediately switches to editable mode."
  (dired (file-name-directory candidate))
  (dired-goto-file candidate)
  (dired-toggle-read-only))

(defun my/helm-find-files-edit ()
  "Exits helm, opens a dired buffer and immediately switches to editable mode."
  (interactive)
  (helm-exit-and-execute-action 'my//helm-find-files-edit))

(defun my/helm-faces ()
  "Describe face."
  (interactive)
  (require 'helm-elisp)
  (let ((default (or (face-at-point) (thing-at-point 'symbol))))
    (helm :sources (helm-def-source--emacs-faces
                    (format "%s" (or default "default")))
          :buffer "*helm faces*")))

(defun my/resume-last-search-buffer ()
  "open last helm-ag or hgrep buffer."
  (interactive)
  (cond ((get-buffer "*helm ag results*")
         (switch-to-buffer-other-window "*helm ag results*"))
        ((get-buffer "*helm-ag*")
         (helm-resume "*helm-ag*"))
        ((get-buffer "*hgrep*")
         (switch-to-buffer-other-window "*hgrep*"))
        (t
         (message "No previous search buffer found"))))

(defun my//helm-do-grep-region-or-symbol
          (&optional targs use-region-or-symbol-p)
        "Version of `helm-do-grep' with a default input."
        (interactive)
        (require 'helm)
        (cl-letf*
            (((symbol-function 'this-fn) (symbol-function 'helm-do-grep-1))
             ((symbol-function 'helm-do-grep-1)
              (lambda (targets &optional recurse zgrep exts
                               default-input region-or-symbol-p)
                (let* ((new-input (when region-or-symbol-p
                                    (if (region-active-p)
                                        (buffer-substring-no-properties
                                         (region-beginning) (region-end))
                                      (thing-at-point 'symbol t))))
                       (quoted-input (when new-input
                                       (rxt-quote-pcre new-input))))
                  (this-fn targets recurse zgrep exts
                           default-input quoted-input))))
             (preselection (or (dired-get-filename nil t)
                               (buffer-file-name (current-buffer))))
             (targets   (if targs
                            targs
                          (helm-read-file-name
                           "Search in file(s): "
                           :marked-candidates t
                           :preselect (if helm-ff-transformer-show-only-basename
                                          (helm-basename preselection)
                                        preselection)))))
          (helm-do-grep-1 targets nil nil nil nil use-region-or-symbol-p)))

(defun my/helm-file-do-grep ()
  "Search in current file with `grep' using a default input."
  (interactive)
  (my//helm-do-grep-region-or-symbol
   (list (buffer-file-name (current-buffer))) nil))

(defun my/helm-file-do-grep-region-or-symbol ()
  "Search in current file with `grep' using a default input."
  (interactive)
  (my//helm-do-grep-region-or-symbol
   (list (buffer-file-name (current-buffer))) t))

(defun my/helm-files-do-grep ()
  "Search in files with `grep'."
  (interactive)
  (my//helm-do-grep-region-or-symbol nil nil))

(defun my/helm-files-do-grep-region-or-symbol ()
  "Search in files with `grep' using a default input."
  (interactive)
  (my//helm-do-grep-region-or-symbol nil t))

(defun my/helm-buffers-do-grep ()
  "Search in opened buffers with `grep'."
  (interactive)
  (let ((buffers (cl-loop for buffer in (buffer-list)
                          when (buffer-file-name buffer)
                          collect (buffer-file-name buffer))))
    (my//helm-do-grep-region-or-symbol buffers nil)))

(defun my/helm-buffers-do-grep-region-or-symbol ()
  "Search in opened buffers with `grep' with a default input."
  (interactive)
  (let ((buffers (cl-loop for buffer in (buffer-list)
                          when (buffer-file-name buffer)
                          collect (buffer-file-name buffer))))
    (my//helm-do-grep-region-or-symbol buffers t)))

(defmacro my||set-helm-key (keys func)
  "Define a key bindings for FUNC using KEYS.
Ensure that helm is required before calling FUNC."
  (let ((func-name (intern (format "my/%s" (symbol-name func)))))
    `(progn
       (defun ,func-name ()
         ,(format "Wrapper to ensure that `helm' is loaded before calling %s."
                  (symbol-name func))
         (interactive)
         (require 'helm)
         (call-interactively ',func))
       (my/set-leader-keys ,keys ',func-name))))


(defun my//helm-do-ag-region-or-symbol (func &optional dir)
  "Search with `ag' with a default input."
  (require 'helm-ag)
  (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
             ;; make thing-at-point choosing the active region first
             ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
             ((symbol-function 'thing-at-point)
              (lambda (thing)
                (let ((res (if (region-active-p)
                               (buffer-substring-no-properties
                                (region-beginning) (region-end))
                             (this-fn thing))))
                  (when res (rxt-quote-pcre res))))))
    (funcall func dir)))

(defun my//helm-do-search-find-tool (base tools default-inputp)
  "Create a cond form given a TOOLS string list and evaluate it."
  (eval
   `(cond
     ,@(mapcar
        (lambda (x)
          `((executable-find ,x)
            ',(let ((func
                     (intern
                      (format (if default-inputp
                                  "my/%s-%s-region-or-symbol"
                                "my/%s-%s")
                              base x))))
                (if (fboundp func)
                    func
                  (intern (format "%s-%s"  base x))))))
        tools)
     (t 'helm-do-grep))))

;; Search in current file ----------------------------------------------

(defun my/helm-file-do-ag (&optional _)
  "Wrapper to execute `helm-ag-this-file.'"
  (interactive)
  (helm-ag-this-file))

(defun my/helm-file-do-ag-region-or-symbol ()
  "Search in current file with `ag' using a default input."
  (interactive)
  (my//helm-do-ag-region-or-symbol 'my/helm-file-do-ag))

(defun my/helm-file-smart-do-search (&optional default-inputp)
  "Search in current file using `dotfile-search-tools'.
Search for a search tool in the order provided by `dotfile-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
  (interactive)
  (call-interactively
   (my//helm-do-search-find-tool "helm-file-do"
                                 dotfile-search-tools
                                 default-inputp)))

(defun my/helm-file-smart-do-search-region-or-symbol ()
  "Search in current file using `dotfile-search-tools' with
 default input.
Search for a search tool in the order provided by `dotfile-search-tools'."
  (interactive)
  (my/helm-file-smart-do-search t))

;; Search in files -----------------------------------------------------

(defun my/helm-files-do-ag (&optional dir)
  "Search in files with `ag' using a default input."
  (interactive)
  (helm-do-ag dir))

(defun my/helm-files-do-ag-region-or-symbol ()
  "Search in files with `ag' using a default input."
  (interactive)
  (my//helm-do-ag-region-or-symbol 'my/helm-files-do-ag))

(defun my/helm-files-smart-do-search (&optional default-inputp)
  "Search in opened buffers using `dotfile-search-tools'.
Search for a search tool in the order provided by `dotfile-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
  (interactive)
  (call-interactively
   (my//helm-do-search-find-tool "helm-files-do"
                                 dotfile-search-tools
                                 default-inputp)))

(defun my/helm-files-smart-do-search-region-or-symbol ()
  "Search in opened buffers using `dotfile-search-tools'.
with default input.
Search for a search tool in the order provided by `dotfile-search-tools'."
  (interactive)
  (my/helm-files-smart-do-search t))

;; Search in buffers ---------------------------------------------------

(defun my/helm-buffers-do-ag (&optional _)
  "Wrapper to execute `helm-ag-buffers.'"
  (interactive)
  (helm-do-ag-buffers))

(defun my/helm-buffers-do-ag-region-or-symbol ()
  "Search in opened buffers with `ag' with a default input."
  (interactive)
  (my//helm-do-ag-region-or-symbol 'my/helm-buffers-do-ag))

(defun my/helm-buffers-smart-do-search (&optional default-inputp)
  "Search in opened buffers using `dotfile-search-tools'.
Search for a search tool in the order provided by `dotfile-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
  (interactive)
  (call-interactively
   (my//helm-do-search-find-tool "helm-buffers-do"
                                 dotfile-search-tools
                                 default-inputp)))

(defun my/helm-buffers-smart-do-search-region-or-symbol ()
  "Search in opened buffers using `dotfile-search-tools' with
default input.
Search for a search tool in the order provided by `dotfile-search-tools'."
  (interactive)
  (my/helm-buffers-smart-do-search t))

;; Search in project ---------------------------------------------------

(defun my/helm-project-do-ag ()
  "Search in current project with `ag'."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (helm-do-ag dir)
      (message "error: Not in a project."))))

(defun my/helm-project-do-ag-region-or-symbol ()
  "Search in current project with `ag' using a default input."
  (interactive)
  (let ((dir (projectile-project-root)))
    (if dir
        (my//helm-do-ag-region-or-symbol 'helm-do-ag dir)
      (message "error: Not in a project."))))

(defun my/helm-project-smart-do-search (&optional default-inputp)
  "Search in current project using `dotfile-search-tools'.
Search for a search tool in the order provided by `dotfile-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
  (interactive)
  (let ((projectile-require-project-root nil))
    (call-interactively
     (my//helm-do-search-find-tool "helm-project-do"
                                   dotfile-search-tools
                                   default-inputp))))

(defun my/helm-project-smart-do-search-region-or-symbol ()
  "Search in current project using `dotfile-search-tools' with
 default input.
Search for a search tool in the order provided by `dotfile-search-tools'."
  (interactive)
  (my/helm-project-smart-do-search t))


(defun my/helm-swoop-region-or-symbol ()
  "Call `helm-swoop' with default input."
  (interactive)
  (let ((helm-swoop-pre-input-function
         (lambda ()
           (if (region-active-p)
               (buffer-substring-no-properties (region-beginning)
                                               (region-end))
             (let ((thing (thing-at-point 'symbol t)))
               (if thing thing ""))))))
    (call-interactively 'helm-swoop)))

(defun my//basic/ui/helm/init ()
  (setq helm-prevent-escaping-from-minibuffer t
        helm-bookmark-show-location t
        helm-display-header-line nil
        helm-split-window-in-side-p t
        helm-always-two-windows t
        helm-echo-input-in-header-line t
        helm-imenu-execute-action-at-once-if-one nil
        helm-org-format-outline-path t
        helm-display-function 'my//display-helm-window)
  (my|hide-lighter helm-mode)
  (add-hook 'helm-minibuffer-set-up-hook 'my//helm-hide-minibuffer-maybe)
  (add-hook 'helm-before-initialize-hook 'my/helm-toggle-header-line)
  (my/add-to-hook 'helm-after-initialize-hook
                  '(my//helm-prepare-display
                    my//hide-cursor-in-helm-buffer))
  (add-hook 'helm-find-files-before-init-hook
            'my//set-dotted-directory)
  (my//helm-hjkl-navigation)

  (advice-add 'helm-make-source :around #'my//helm-make-source)
  (defadvice my/post-theme-init
      (after my/helm-header-line-adv activate)
    "Update defaults for `helm' header line whenever a new theme is loaded"
    ;; TODO factorize face definition with those defined in config.el
    (setq helm-source-header-default-foreground
          (face-attribute 'helm-source-header :foreground)
          helm-source-header-default-background
          (face-attribute 'helm-source-header :background)
          helm-source-header-default-box
          (face-attribute 'helm-source-header :box)
          helm-source-header-default-height
          (face-attribute 'helm-source-header :height)))
  (my|define-transient-state helm-navigation
                                    :title "Helm Transient State"
                                    :doc "
 [_j_/_k_]  next/prev candidate  [_v_]^^     persistent action     [_e_]^^    edit occurrences
 [_h_/_l_]  prev/next source     [_1_.._0_]  action 1..10          [_t_/_T_]  toggle visible/all mark
 [_q_]^^    quit                 [_a_]^^     action selection pg"
                                    :foreign-keys run
                                    :on-enter (my//helm-navigation-ts-on-enter)
                                    :on-exit  (my//helm-navigation-ts-on-exit)
                                    :bindings
                                    ("1" my/helm-action-1 :exit t)
                                    ("2" my/helm-action-2 :exit t)
                                    ("3" my/helm-action-3 :exit t)
                                    ("4" my/helm-action-4 :exit t)
                                    ("5" my/helm-action-5 :exit t)
                                    ("6" my/helm-action-6 :exit t)
                                    ("7" my/helm-action-7 :exit t)
                                    ("8" my/helm-action-8 :exit t)
                                    ("9" my/helm-action-9 :exit t)
                                    ("0" my/helm-action-10 :exit t)
                                    ("<tab>" helm-select-action :exit t)
                                    ("TAB" helm-select-action :exit t)
                                    ("<RET>" helm-maybe-exit-minibuffer :exit t)
                                    ("a" my/helm-transient-state-select-action)
                                    ("e" my/helm-ts-edit)
                                    ("g" helm-beginning-of-buffer)
                                    ("G" helm-end-of-buffer)
                                    ("h" helm-previous-source)
                                    ("j" helm-next-line)
                                    ("k" helm-previous-line)
                                    ("l" helm-next-source)
                                    ("q" nil :exit t)
                                    ("t" helm-toggle-visible-mark)
                                    ("T" helm-toggle-all-marks)
                                    ("v" helm-execute-persistent-action))
  (define-key helm-map (kbd "M-SPC")
    'my/helm-navigation-transient-state/body)
  (define-key helm-map (kbd "s-M-SPC")
    'my/helm-navigation-transient-state/body)
  (with-eval-after-load 'helm-files
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-find-files-map
      (kbd "S-<tab>") 'helm-find-files-up-one-level)
    (define-key helm-find-files-map
      (kbd "<backtab>") 'helm-find-files-up-one-level)
    ;; For terminal.
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-find-files-map
      (kbd "S-TAB") 'helm-find-files-up-one-level)
    (define-key helm-map (kbd "C-z") 'helm-select-action)
    (define-key helm-find-files-map
          (kbd "C-c C-e") 'my/helm-find-files-edit))
  (define-key helm-map (kbd "C-q") 'ace-jump-helm-line)
  (my/set-leader-keys "fb" 'helm-filtered-bookmarks)
  (my/add-to-hook 'helm-cleanup-hook
                  '(my//restore-previous-display-config
                    my//helm-cleanup))
  (evil-ex-define-cmd "buffers" 'helm-buffers-list)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'my/helm-find-files)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)

  (my/set-leader-keys
        "<f1>" 'helm-apropos
        "a'"   'helm-available-repls
        "bb"   'helm-mini
        "Cl"   'helm-colors
        "ff"   'my/helm-find-files
        "fF"   'helm-find-files
        "fL"   'helm-locate
        "fr"   'helm-recentf
        "hdd"  'helm-apropos
        "hdF"  'my/helm-faces
        "hi"   'helm-info-at-point
        "hm"   'helm-man-woman
        "iu"   'helm-ucs
        "jI"   'helm-imenu-in-all-buffers
        "rm"   'helm-all-mark-rings
        "rl"   'helm-resume
        "rr"   'helm-register
        "rs"   'my/resume-last-search-buffer
        "ry"   'helm-show-kill-ring
        "sl"   'my/resume-last-search-buffer
        "sj"   'my/jump-in-buffer)

  (my/set-leader-keys
        "sgb"  'my/helm-buffers-do-grep
        "sgB"  'my/helm-buffers-do-grep-region-or-symbol
        "sgf"  'my/helm-files-do-grep
        "sgF"  'my/helm-files-do-grep-region-or-symbol
        "sgg"  'my/helm-file-do-grep
        "sgG"  'my/helm-file-do-grep-region-or-symbol)

  (my||set-helm-key "fel" helm-locate-library)
  (my||set-helm-key "hdm" describe-mode)
  (my||set-helm-key "sww" helm-wikipedia-suggest)
  (my||set-helm-key "swg" helm-google-suggest)

  (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
  (add-hook 'emacs-startup-hook
            (lambda ()
                (my/set-leader-keys dotfile-emacs-command-key 'helm-M-x)))

  (helm-mode)
  (helm-locate-set-command)
  (setq helm-locate-fuzzy-match (string-match "locate" helm-locate-command))
  (defun simpler-helm-bookmark-keybindings ()
    (define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
    (define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)
    (define-key helm-bookmark-map
      (kbd "C-f") 'helm-bookmark-toggle-filename)
    (define-key helm-bookmark-map
      (kbd "C-o") 'helm-bookmark-run-jump-other-window)
    (define-key helm-bookmark-map (kbd "C-/") 'helm-bookmark-help))
  (add-hook 'helm-mode-hook 'simpler-helm-bookmark-keybindings)
  (my|hide-lighter helm-mode)

  (defun my/helm-project-smart-do-search-in-dir (dir)
    (interactive)
    (let ((default-directory dir))
      (my/helm-project-smart-do-search)))
  (define-key helm-projectile-projects-map
    (kbd "C-s")
    (lambda ()
      (interactive)
      (helm-exit-and-execute-action
       'my/helm-project-smart-do-search-in-dir)))

  (evilified-state-evilify helm-grep-mode helm-grep-mode-map
                           (kbd "RET") 'helm-grep-mode-jump-other-window
                           (kbd "q") 'quit-window)

  (my/set-leader-keys
   ;; helm-ag marks
   "s`"  'helm-ag-pop-stack
   ;; opened buffers scope
   "sb"  'my/helm-buffers-smart-do-search
   "sB"  'my/helm-buffers-smart-do-search-region-or-symbol
   "sab" 'helm-do-ag-buffers
   "saB" 'my/helm-buffers-do-ag-region-or-symbol
   ;; current file scope
   "ss"  'my/helm-file-smart-do-search
   "sS"  'my/helm-file-smart-do-search-region-or-symbol
   "saa" 'helm-ag-this-file
   "saA" 'my/helm-file-do-ag-region-or-symbol
   ;; files scope
   "sf"  'my/helm-files-smart-do-search
   "sF"  'my/helm-files-smart-do-search-region-or-symbol
   "saf" 'helm-do-ag
   "saF" 'my/helm-files-do-ag-region-or-symbol
   ;; current project scope
   "/"   'my/helm-project-smart-do-search
   "*"   'my/helm-project-smart-do-search-region-or-symbol
   "sp"  'my/helm-project-smart-do-search
   "sP"  'my/helm-project-smart-do-search-region-or-symbol
   "sap" 'my/helm-project-do-ag
   "saP" 'my/helm-project-do-ag-region-or-symbol)

  (evil-define-key 'normal helm-ag-map "SPC" my-default-map)
  (evilified-state-evilify helm-ag-mode helm-ag-mode-map
                           (kbd "RET") 'helm-ag-mode-jump-other-window
                           (kbd "gr") 'helm-ag--update-save-results
                           (kbd "q") 'quit-window)

  (setq helm-descbinds-window-style 'split)
  (add-hook 'helm-mode-hook 'helm-descbinds-mode)
  (my/set-leader-keys "?" 'helm-descbinds)

  (setq helm-flx-for-helm-find-files nil)
  (helm-flx-mode)

  (my/set-leader-keys "cc" 'helm-make-projectile
                      "cm" 'helm-make)

  (my/set-leader-keys "hM"    'helm-switch-major-mode
                      ;; "hm"    'helm-disable-minor-mode
                      "h C-m" 'helm-enable-minor-mode)

  (defalias 'my/helm-project-do-grep 'helm-projectile-grep)
  (defalias 'my/helm-project-do-grep-region-or-symbol 'helm-projectile-grep)
  (setq projectile-switch-project-action 'helm-projectile)
  (my/set-leader-keys "pb"  'helm-projectile-switch-to-buffer
                      "pd"  'helm-projectile-find-dir
                      "pf"  'helm-projectile-find-file
                      "pF"  'helm-projectile-find-file-dwim
                      "ph"  'helm-projectile
                      "pp"  'helm-projectile-switch-project
                      "pr"  'helm-projectile-recentf
                      "sgp" 'helm-projectile-grep)

  (setq helm-swoop-split-with-multiple-windows t
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-speed-or-color t
        helm-swoop-split-window-function 'helm-default-display-buffer
        helm-swoop-pre-input-function (lambda () ""))

  (my/set-leader-keys "ss"    'helm-swoop
                      "sS"    'my/helm-swoop-region-or-symbol
                      "s C-s" 'helm-multi-swoop-all)
  (defadvice helm-swoop (before add-evil-jump activate) (evil-set-jump))

  (my/set-leader-keys "Ts" 'helm-themes))

(provide 'my//basic/ui/helm)
