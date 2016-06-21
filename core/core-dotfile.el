(defconst dotfile-directory user-emacs-directory
  "Optional dotfile directory.")

(defconst dotfile-filepath (concat dotfile-directory "dotfile.el")
  "Filepath to the dotfile.")

(defvar dotfile-themes '(solarized-dark)
  "List of themes.")

(defvar dotfile-colorized-cursor-according-to-state t
  "If non nil the cursor color matches the state color in GUI Emacs.")

(defvar dotfile-leader-key "SPC"
  "The leader key.")

(defvar dotfile-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar dotfile-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which the equivalent of pressing `<leader> m'.")

(defvar dotfile-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar dotfile-emacs-command-key "SPC"
  "The key used for Emacs commands (M-x) (after pressing the leader key).")

(defvar dotfile-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal)
  "Default font, or prioritized list of fonts.")

(defvar dotfile-which-key-delay 0.4
  "Delay in second starting from the last keystroke after which the which-key buffer will e show.")

(defvar dotfile-fullscreen-at-startup nil
  "If non-nil the frame is fullscreen on startup.")


(defun dotfile/load-file ()
  "Loader dotfile if it exists."
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
