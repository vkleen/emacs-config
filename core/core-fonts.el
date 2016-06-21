(require 'core-funcs)

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

(provide 'core-fonts)
