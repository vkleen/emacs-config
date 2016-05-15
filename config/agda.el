;; Agda is presumed to be in PATH
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-function-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-postulate-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-primitive-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "deep sky blue"))))
 '(agda2-highlight-record-face ((t (:foreground "deep sky blue")))))
