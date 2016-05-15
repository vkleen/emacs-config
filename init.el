
;; Standard libraries needed

(require 'cl-lib)


;; Packages and configs to load

(setq package-enable-at-startup nil)
(package-initialize)

(defvar packages
  '(haskell-mode
    smex
    dash
    writeroom-mode
    dired+
    yasnippet)
  "Packages whose location follows the
  packages/package-name/package-name.el format.")

(defvar custom-load-paths
  '("structured-haskell-mode/elisp"
    "hindent/elisp"
    "git-modes"
    "auctex-latexmk"
    "visual-fill-column"
    "cdlatex"
    "magit/lisp")
  "Custom load paths that don't follow the normal
  package-name/module-name.el format.")

(defvar configs
  '("global"
    "haskell"
    "notmuch"
    "latex"
    "org"
    "cdlatex"
    "yasnippet"
    "magit"
    "agda"
    "idris")
  "Configuration files that follow the config/foo.el file path
  format.")

(defvar themes
  '("solarized-emacs")
  "Theme directories that follow the themes/theme-name file path format.")


;; Load packages

(cl-loop for location in custom-load-paths
         do (add-to-list 'load-path
                         (concat (file-name-directory (or load-file-name
                                                          (buffer-file-name)))
                                 "packages/"
                                 location)))

(cl-loop for name in packages
         do (progn (unless (fboundp name)
                     (add-to-list 'load-path
                                  (concat (file-name-directory (or load-file-name
                                                                   (buffer-file-name)))
                                          "packages/"
                                          (symbol-name name)))
                     (require name))))

(require 'shm)
(require 'hindent)
(require 'shm-case-split)
(require 'shm-reformat)


;; Theme paths

(cl-loop for location in themes
         do (progn (add-to-list 'custom-theme-load-path
                                (concat (file-name-directory (or load-file-name
                                                                 (buffer-file-name)))
                                        "themes/"
                                        location))
                   (add-to-list 'load-path
                                (concat (file-name-directory (or load-file-name
                                                                 (buffer-file-name)))
                                        "themes/"
                                        location))))


;; Emacs configurations

(cl-loop for name in configs
         do (load (concat (file-name-directory load-file-name)
                          "config/"
                          name ".el")))


;; Mode initializations

(smex-initialize)
(load "haskell-mode-autoloads.el")
(require 'nix-mode)
