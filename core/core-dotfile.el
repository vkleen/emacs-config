(defconst dotfile-directory user-emacs-directory
  "Optional dotfile directory.")

(defconst dotfile-filepath (concat dotfile-directory "dotfile.el")
  "Filepath to the dotfile.")

(defvar dotfile-colorize-cursor-according-to-state t
  "If non nil the cursor color matches the state color in GUI Emacs.")

(defvar dotfile-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal)
  "Default font, or prioritized list of fonts.")

(defun dotfile/load-file ()
  "Load dotfile if it exists."
  (if (file-exists-p dotfile-filepath)
      (with-demoted-errors "Error loading dotfile: %S" (load dotfile-filepath))))

(defmacro dotfile|call-func (func &optional msg)
  "Call the function from the dotfile only if it is bound. If `msg' is not nil then display a message in `*Messages*'. Errors are caught and signalled to the user."
  `(progn (when ,msg (my/message ,msg))
          (when (fboundp ',func)
            (condition-case-unless-debug err
                (,func)
              (my/error "In %s: %s\n" ',(symbol-name func) (error-message-string err))))))

(provide 'core-dotfile)
