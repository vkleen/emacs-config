(require 'subr-x)
(require 'f)
(require 'ht)
(require 'dash)
(require 'core-funcs)

(defvar my-layer-file-types '((:config . "config.el")
                           (:packages . "packages.el"))
  "Alist of known file-types in layer directories. These are
  available for overriding in calls to `my/discover-layers'.")

(defvar my-layer-paths (make-hash-table :size 256)
  "Hash table of layer locations. Each entry is a plist of files
  associated with the layer.")

(defun my//layer-directory? (path)
  "Is PATH a layer directory?"
  (and (f-directory? path)
       (f-exists? (f-join path "this-is-a-layer"))))

(defun my//make-layer-plist (path)
  "Make a layer file plist from PATH. Assumes PATH is already
checked to be layer directory."
  (let ((result))
    (dolist (type my-layer-file-types)
      (-let [(key . rel-path) type]
        (when (f-exists? (f-join path rel-path))
          (my/append-to-list 'result (list key (f-join path rel-path))))))
    result))

(defun my//merge-layer-plists (plist1 plist2)
  "Merge the layer file plists PLIST1 and PLIST2, giving precedence to PLIST2."
  (let ((result (copy-sequence plist1))
        k v)
    (while plist2
      (setq k (pop plist2)
            v (pop plist2)
            result (plist-put result k v)))
    result))

(defun my//make-layer-symbol (path layer-path)
  "Generate the layer symbol from its path LAYER-PATH when it was
found while searching PATH."
  (intern (f-relative layer-path path)))

(defun my/discover-layers (path)
  "Discover layers in PATH. Fills into hash table `my-layer-paths'."
  (when (f-directory? path)
      (dolist (layer-path (f-directories path 'my//layer-directory? t))
        (my/debug-message "Discovered layer in `%s'" layer-path)
        (let ((layer-plist (my//make-layer-plist layer-path))
              (layer-symbol (my//make-layer-symbol path layer-path)))
          (-if-let (orig-plist (ht-get my-layer-paths layer-symbol))
              (ht-set! my-layer-paths
                       layer-symbol
                       (my//merge-layer-plists orig-plist layer-plist))
            (ht-set! my-layer-paths layer-symbol layer-plist))))))

(defun my/debug-print-discovered-layers ()
  "Debug print all discovered layers."
  (ht-each (lambda (k v)
             (my/debug-message "Layer `%s'" k)
             (my/debug-message "-> %s" v))
           my-layer-paths))

(defmacro my|use-package (pkg &rest body)
  "Wrap use-package to always use quelpa."
  `(use-package pkg :quelpa ,@body))

(provide 'core-layers)
