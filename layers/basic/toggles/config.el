(defvar my-toggles '()
    "List of all declared toggles. The structure of an element is
a property list (name :func FUNCTION :doc STRING :key STRING).")

(defmacro my|add-toggle (name &rest props)
  "Add a toggle with NAME symbol.

This macro create the following functions:
- my/basic/toggle/NAME switches on or off depending on the current state
- my/basic/toggle/NAME-on only switches on if currently disabled
- my/basic/toggle/NAME-off only switches off if currently enabled

Avaiblabe PROPS:

`:status EXPRESSION'
    The EXPRESSION to evaluate to get the current status of the toggle.

`:if EXPRESSION'
    If this EXPRESSION evaluate to nil then no attempt to update the toggle
    status will be performed.

`:on BODY'
    Evaluate BODY when the toggle is switched on.

`:off BODY'
    Evaluate BODY when the toggle is switched off.

`:documentation STRING'
    STRING describes what the toggle does.

`:prefix SYMBOL'
    SYMBOL is bound to the raw value of prefix-arg (same as calling
    (interactive \"P\")) in the wrapper function.

`:on-message EXPRESSION'
    EXPRESSION is evaluated and displayed when the \"on\" toggle is activated.

`:mode SYMBOL'
    If given, must be a minor mode. This overrides `:on', `:off' and `:status'.

All properties supported by `my//create-key-binding-form' can be
used."
  (declare (indent 1))
  (let* ((wrapper-func (intern (format "my/basic/toggle/%s" (symbol-name name))))
         (wrapper-func-status (intern (format "%s-p" wrapper-func)))
         (wrapper-func-on (intern (format "%s-on" wrapper-func)))
         (wrapper-func-off (intern (format "%s-off" wrapper-func)))
         (mode (plist-get props :mode))
         (status (or mode (plist-get props :status)))
         (condition (plist-get props :if))
         (doc (plist-get props :documentation))
         (on-body (if mode `((,mode)) (my/mplist-get props :on)))
         (off-body (if mode `((,mode -1)) (my/mplist-get props :off)))
         (prefix-arg-var (plist-get props :prefix))
         (on-message (plist-get props :on-message))
         (bindkeys (my//create-key-binding-form props wrapper-func))
         (status-eval `(and (or (and (symbolp ',status) (boundp ',status))
                                (listp ',status))
                            ,status)))
    `(progn
       (push (append '(,name) '(:function ,wrapper-func
                                          :predicate ,wrapper-func-status) ',props)
             my-toggles)
       (defun ,wrapper-func ,(if prefix-arg-var (list prefix-arg-var) ())
         ,(format "Toggle %s on and off." (symbol-name name))
         ,(if prefix-arg-var '(interactive "P") '(interactive))
         (if (or (null ',condition)
                 (and (or (and (symbolp ',condition) (boundp ',condition))
                          (listp ',condition))
                      ,condition))
             (if (,wrapper-func-status)
                 (progn ,@off-body
                        (message ,(format "%s disabled." name)))
               ,@on-body
               (message ,(or on-message (format "%s enabled." name))))
           (message "This toggle is not supported.")))

       (defun ,wrapper-func-status ()
         ,(format "Check if %s is on." (symbol-name name))
         ,status-eval)

       ,@(when status
           `((defun ,wrapper-func-on ()
               ,(format "Enable %s." (symbol-name name))
               (interactive)
               (unless (,wrapper-func-status) (,wrapper-func)))
             (defun ,wrapper-func-off ()
               ,(format "Disable %s." (symbol-name name))
               (interactive)
               (when (,wrapper-func-status) (,wrapper-func)))))
       ,@bindkeys)))

(my/add-which-key-description "my/basic/toggle/\\(.+\\)" "\\1")
