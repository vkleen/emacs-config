(setq load-prefer-newer t
      debug-on-error t
      package-archives nil
      gc-cons-threshold (* 50 1024 1024))

(defconst user-emacs-directory
  (file-name-directory (expand-file-name (or load-file-name (buffer-file-name))))
  "My emacs config directory.")

(defconst quelpa-src-directory
  (file-name-as-directory (expand-file-name "modules/quelpa" user-emacs-directory))
  "Where quelpa is stored in the system.")

(defconst user-cache-directory
  (file-name-as-directory (expand-file-name ".cache" user-emacs-directory))
  "My emacs storage area for persistent files.")
(unless (file-exists-p user-cache-directory)
  (make-directory user-cache-directory))

(setenv "GIT_SSL_CAINFO" "/etc/ssl/certs/ca-certificates.crt")
(setq quelpa-ci-dir quelpa-src-directory)
(if (require 'quelpa nil t)
    (quelpa '(quelpa :repo quelpa-src-directory :fetcher git) :upgrade t)
  (load (expand-file-name "bootstrap.el" quelpa-src-directory)))

(setq quelpa-dir (expand-file-name "quelpa" user-cache-directory)
      quelpa-melpa-repo-url (expand-file-name "modules/melpa"
                                              user-emacs-directory))

(quelpa 'f)
(require 'f)

(defconst user-layer-directory
  (f-join user-emacs-directory "layers")
  "My spacemacs-inspired layers.")

(setq my-show-debug-messages t)
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(require 'core-init)
(my/Init)

(my/debug-print-discovered-layers)
