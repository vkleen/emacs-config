(defun helm-gtags-dwim-other-window ()
  "helm-gtags-dwim in the other window"
  (interactive)
  (let ((helm-gtags--use-otherwin t)
        (split-height-threshold nil)
        (split-width-threshold 140))
    (helm-gtags-dwim)))

(defun my/helm-gtags-define-keys-for-mode (mode)
  "Define key bindings for the specific MODE."
  (when (fboundp mode)
    (let ((hook (intern (concat (symbol-name mode) "-hook"))))
      (add-hook hook 'helm-gtags-mode))
    (my/set-leader-keys-for-major-mode mode
      "gc" 'helm-gtags-create-tags
      "gd" 'helm-gtags-find-tag
      "gD" 'helm-gtags-find-tag-other-window
      "gf" 'helm-gtags-select-path
      "gg" 'helm-gtags-dwim
      "gG" 'helm-gtags-dwim-other-window
      "gi" 'helm-gtags-tags-in-this-function
      "gl" 'helm-gtags-parse-file
      "gn" 'helm-gtags-next-history
      "gp" 'helm-gtags-previous-history
      "gr" 'helm-gtags-find-rtag
      "gR" 'helm-gtags-resume
      "gs" 'helm-gtags-select
      "gS" 'helm-gtags-show-stack
      "gu" 'helm-gtags-update-tags)))

(defun my/ggtags-mode-enable ()
  "Enable ggtags and eldoc mode.

For eldoc, ggtags advises the eldoc function at the lowest priority so that if the major mode has better support it will use it first."
  (ggtags-mode 1)
  (eldoc-mode 1))

(provide 'my//tags/ggtags/funcs)
