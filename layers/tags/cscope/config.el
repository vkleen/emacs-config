(setq cscope-option-do-not-update-database t
      cscope-display-cscope-buffer nil)

(defun cscope//safe-project-root ()
  "Return project's root, or nil if not in a project."
  (and (fboundp 'projectile-project-root)
       (projectile-project-p)
       (projectile-project-root)))

(defun cscope/run-pycscope (directory)
  (interactive (list (file-name-as-directory
                      (read-directory-name "Run pycscope in directroy: "
                                           (cscope//safe-project-root)))))
  (let ((default-directory directory))
    (shell-command
     (format "pycscope -R -f '%s'"
             (expand-file-name "cscope.out" directory)))))

(defun my/setup-helm-cscope (mode)
  "Setup `helm-cscope' for MODE"
  (my/set-leader-keys-for-major-mode mode
    "gc" 'helm-cscope-find-called-function
    "gC" 'helm-cscope-find-calling-this-funtcion
    "gd" 'helm-cscope-find-global-definition
    "ge" 'helm-cscope-find-egrep-pattern
    "gf" 'helm-cscope-find-this-file
    "gF" 'helm-cscope-find-files-including-file
    "gr" 'helm-cscope-find-this-symbol
    "gx" 'helm-cscope-find-this-text-string))
(defadvice helm-cscope-find-this-symbol (before cscope/goto activate)
  (evil--jumps-push))
