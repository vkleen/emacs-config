(defvar my--diminished-minor-modes nil
  "List of diminished mode to short characters.")

(defmacro my|diminish (mode &optional short)
  "Diminish MODE name in mode line to SHORT. If SHORT is not provided, the mode will not show in the mode line."
  `(let ((cell (assq ',mode my--diminished-minor-modes)))
     (if cell
         (setcdr cell ',short)
       (push '(,mode ,short) my--diminished-minor-modes))))

(defmacro my|hide-lighter (mode)
  "Diminish MODE name in mode line to LIGHTER."
  `(eval-after-load 'diminish '(diminish ',mode)))

(provide 'core-diminish)
