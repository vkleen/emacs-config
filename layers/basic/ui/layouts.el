(defvar dotfile-auto-resume-layouts t
  "If non nil then the last auto saved layouts are resume automatically upon
start.")

(defvar my-layouts-directory
  (expand-file-name (f-join user-cache-directory "layouts"))
  "Save layouts in this directory.")

(defvar layouts-enable-autosave nil
  "If true, saves perspectives to file per `layouts-autosave-delay'")

(defvar layouts-autosave-delay 900
  "Delay in seconds between each layouts auto-save.")

(defvar my--ts-full-hint-toggle 0
  "Toggle display of transient states documentations.")

(defvar my--last-selected-layout "Default"
  "Previously selected layout.")

(defvar my--custom-layout-alist nil
  "List of custom layouts with their bound keys.
 Do not modify directly, use provided `my|define-custom-layout'")

(defvar my--layouts-autosave-timer nil
  "Timer for layouts auto-save.")

(defun my-layouts/non-restricted-buffer-list ()
  (interactive)
  (remove-hook 'ido-make-buffer-list-hook  #'persp-restrict-ido-buffers)
  (helm-mini)
  (add-hook 'ido-make-buffer-list-hook  #'persp-restrict-ido-buffers))

(defun my//workspace-format-name (workspace)
  "Return a porpertized string given a WORKSPACE name."
  (let* ((current (eq (eyebrowse--get 'current-slot) (car workspace)))
         (name (nth 2 workspace))
         (number (car workspace))
         (caption (if (< 0 (length name))
                      (concat (int-to-string number) ":" name)
                    (int-to-string number))))
    (if current
        (propertize (concat "[" caption "]") 'face 'warning)
      caption)))

(defun my//workspaces-ts-hint ()
  "Return a one liner string containing all the workspaces names."
  (concat
   " "
   (mapconcat 'my//workspace-format-name
              (eyebrowse--get 'window-configs) " | ")

(if (equal 1 my--ts-full-hint-toggle)
       my--workspaces-ts-full-hint
     (concat "  (["
             (propertize "?" 'face 'hydra-face-red)
             "] help)"))))

(defun my//workspaces-ts-toggle-hint ()
  "Toggle the full hint docstring for the workspaces transient-state."
  (interactive)
  (setq my--ts-full-hint-toggle

        (logxor my--ts-full-hint-toggle 1)))

(defun my/workspaces-ts-rename ()
  "Rename a workspace and get back to transient-state."
  (interactive)
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) nil)
  (my/workspaces-transient-state/body))

(defun my/window-state-window-p (object)
  "Return t if OBJECT is a window, as represented in window-state objects.
Note: this function doesn't test for real window objects, but for
representations of a window in a window-state object as returned by
`window-state-get'."
  (and (listp object)
       (memq (car object) '(leaf vc hc))))

(defun my/window-state-get-buffer (window)
  "Get WINDOW's buffer.
WINDOW is the representation of a window in a window-state object.
The returned value is the representation of a buffer in a window-state
object."
  (cdr (assq 'buffer window)))

(defun my/window-state-get-buffer-name (window)
  "Get WINDOW's buffer's name.
WINDOW is the representation of a window in a window-state object."
  (car (my/window-state-get-buffer window)))

(defun my/window-state-walk-windows-1 (window fn)
  "Helper function for `my/window-state-walk-windows'."
  ;; WINDOW is a misleading name. WINDOW is a list that can represent a window,
  ;; or a concatenation of several windows. window-state objects are weird.
  (let ((child-windows
         (-filter #'my/window-state-window-p window))
        (bare-window
         ;; if WINDOW contains more than one window, take only the first window
         (--take-while (not (my/window-state-window-p it))
                       window)))
    (--each child-windows
      (my/window-state-walk-windows-1 it fn))
    (push (funcall fn bare-window) result)))

(defun my/window-state-walk-windows (state fn)
  "Execute FN once for each window in STATE and make a list of the results.
FN is a function to execute.
STATE is a window-state object."
  (let (result)
    (my/window-state-walk-windows-1 (cdr state) fn)
    result))

(defun my/window-state-all-windows (state)
  "Get all windows contained in STATE.
STATE is a window-state object.
The returned windows are not actual window objects. They are windows as
represented in window-state objects."
  (my/window-state-walk-windows state #'identity))

(defun my/window-state-get-buffer-names (state)
  "Get names of all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  (delq nil (my/window-state-walk-windows state #'my/window-state-get-buffer-name)))

(defun my/window-state-get-buffers (state)
  "Get all buffers saved in STATE.
STATE is a window-state object as returned by `window-state-get'."
  ;; delq nil - removes buffers stored in STATE that don't exist anymore
  (delq nil (mapcar #'get-buffer (my/window-state-get-buffer-names state))))

(defun my/find-workspace (buffer)
  "Find Eyebrowse workspace containing BUFFER.
 If several workspaces contain BUFFER, return the first one. Workspaces are
 ordered by slot number.
 If no workspace contains
 BUFFER, return nil."
  ;; the second element of a workspace is its window-state object
  (--find (memq buffer (my/window-state-get-buffers (cadr it)))
          (eyebrowse--get 'window-configs)))

(defun my/display-in-workspace (buffer alist)
  "Display BUFFER's workspace.
 Return BUFFER's window, if exists, otherwise nil.
 If BUFFER is already visible in current workspace, just return its window
 without switching workspaces."
  (or (get-buffer-window buffer)
      (-when-let (workspace (my/find-workspace buffer))
        (eyebrowse-switch-to-window-config (car workspace))
        (get-buffer-window buffer))))

(defun my/goto-buffer-workspace (buffer)
  "Switch to BUFFER's window in BUFFER's workspace.
 If BUFFER isn't displayed in any workspace, display it in the current
 workspace, preferably in the current window."
  (interactive "B")
  (pop-to-buffer buffer '((;; reuse buffer window from some workspace
                           my/display-in-workspace
                           ;; fallback to display in current window
                           display-buffer-same-window)
                          (inhibit-same-window . nil))))

(defun my//get-persp-workspace (&optional persp frame)
  "Get the correct workspace parameters for perspective.
PERSP is the perspective, and defaults to the current perspective.
FRAME is the frame where the parameters are expected to be used, and
defaults to the current frame."
  (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--map (persp-parameter it persp) param-names)))

(defun my//set-persp-workspace (workspace-params &optional persp frame)
  "Set workspace parameters for perspective.
WORKSPACE-PARAMS should be a list containg 3 elements in this order:
- window-configs, as returned by (eyebrowse--get 'window-configs)
- current-slot, as returned by (eyebrowse--get 'current-slot)
- last-slot, as returned by (eyebrowse--get 'last-slot)
PERSP is the perspective, and defaults to the current perspective.
FRAME is the frame where the parameters came from, and defaults to the
current frame.

Each perspective has two sets of workspace parameters: one set for
graphical frames, and one set for terminal frames."
  (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--zip-with (set-persp-parameter it other persp)
                param-names workspace-params)))

(defun my/load-eyebrowse-for-perspective (type &optional frame)
  "Load an eyebrowse workspace according to a perspective's parameters.
 FRAME's perspective is the perspective that is considered, defaulting to
 the current frame's perspective.
 If the perspective doesn't have a workspace, create one."
  (when (eq type 'frame)
    (let* ((workspace-params (my//get-persp-workspace (get-frame-persp frame) frame))
           (window-configs (nth 0 workspace-params))
           (current-slot (nth 1 workspace-params))
           (last-slot (nth 2 workspace-params)))
      (if window-configs
          (progn
            (eyebrowse--set 'window-configs window-configs frame)
            (eyebrowse--set 'current-slot current-slot frame)
            (eyebrowse--set 'last-slot last-slot frame)
            (eyebrowse--load-window-config current-slot))
        (eyebrowse--set 'window-configs nil frame)
        (eyebrowse-init frame)
        (my/save-eyebrowse-for-perspective frame)))))

(defun my/update-eyebrowse-for-perspective (_new-persp-name _frame)
  "Update and save current frame's eyebrowse workspace to its perspective.
Parameters _NEW-PERSP-NAME and _FRAME are ignored, and exists only for
 compatibility with `persp-before-switch-functions'."
  (let* ((current-slot (eyebrowse--get 'current-slot))
         (current-tag (nth 2 (assoc current-slot (eyebrowse--get 'window-configs)))))
    (eyebrowse--update-window-config-element
     (eyebrowse--current-window-config current-slot current-tag)))
  (my/save-eyebrowse-for-perspective))

(defun my/save-eyebrowse-for-perspective (&optional frame)
  "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
  (my//set-persp-workspace (list (eyebrowse--get 'window-configs frame)
                                 (eyebrowse--get 'current-slot frame)
                                 (eyebrowse--get 'last-slot frame))
                           (get-frame-persp frame)
                           frame))

(defun my/persp-helm-mini ()
  "As `helm-mini' but restricts visible buffers by perspective."
  (interactive)
  (with-persp-buffer-list ()
                          (helm-mini)))

(defun my//helm-perspectives-source ()
  (helm-build-in-buffer-source
      (concat "Current Perspective: " (my//current-layout-name))
    :data (persp-names)
    :fuzzy-match t
    :action
    '(("Switch to perspective" . persp-switch)
      ("Close perspective(s)" . (lambda (candidate)
                                  (mapcar
                                   'persp-kill-without-buffers
                                   (helm-marked-candidates))))
      ("Kill perspective(s)" . (lambda (candidate)
                                 (mapcar 'persp-kill
                                         (helm-marked-candidates)))))))
(defun my/helm-perspectives ()
  "Control Panel for perspectives. Has many actions.
If match is found
f1: (default) Select perspective
f2: Close Perspective(s) <- mark with C-SPC to close more than one-window
f3: Kill Perspective(s)

If match is not found
<enter> Creates perspective

Closing doesn't kill buffers inside the perspective while killing
perspectives does."
  (interactive)
  (helm
   :buffer "*Helm Perspectives*"
   :sources
   `(,(my//helm-perspectives-source)
     ,(helm-build-dummy-source "Create new perspective"
        :requires-pattern t
        :action
        '(("Create new perspective" .
           (lambda (name)
             (let ((persp-reset-windows-on-nil-window-conf t))
               (persp-switch name)
               (unless (member name (persp-names-current-frame-fast-ordered))
                 (my/new-empty-buffer))))))))))

;; ability to use helm find files but also adds to current perspective
(defun my/helm-persp-close ()
  "Kills perspectives without killing the buffers"
  (interactive)
  (helm
   :buffer "*Helm Kill Perspectives (without killing buffers)*"
   :sources
   (helm-build-in-buffer-source
       (concat "Current Perspective: " (my//current-layout-name))
     :data (persp-names)
     :fuzzy-match t
     :action
     '(("Close perspective(s)" . (lambda (candidate)
                                   (mapcar
                                    'persp-kill-without-buffers
                                    (helm-marked-candidates))))))))

(defun my/helm-persp-kill ()
  "Kills perspectives with all their buffers"
  (interactive)
  (helm
   :buffer "*Helm Kill Perspectives with all their buffers*"
   :sources (helm-build-in-buffer-source
                (s-concat "Current Perspective: "
                          (my//current-layout-name))
              :data (persp-names)
              :fuzzy-match t
              :action
              '(("Kill perspective(s)" .
                 (lambda (candidate)
                   (mapcar 'persp-kill
                           (helm-marked-candidates))))))))

(defun my/helm-persp-switch-project (arg)
  (interactive "P")
  (helm
   :sources
   (helm-build-in-buffer-source "*Helm Switch Project Layout*"
     :data (lambda ()
             (if (projectile-project-p)
                 (cons (abbreviate-file-name (projectile-project-root))
                       (projectile-relevant-known-projects))
               projectile-known-projects))
     :fuzzy-match helm-projectile-fuzzy-match
     :mode-line helm-read-file-name-mode-line-string
     :action '(("Switch to Project Perspective" .
                (lambda (project)
                  (let ((persp-reset-windows-on-nil-window-conf t))
                    (persp-switch project)
                    (let ((projectile-completion-system 'helm))
                      (projectile-switch-project-by-name project)))))))
   :buffer "*Helm Projectile Layouts*"))

(defun my//activate-persp-mode ()
        "Always activate persp-mode, unless it is already active."
        (unless (bound-and-true-p persp-mode)
          (persp-mode)))

(defun my//layouts-ts-toggle-hint ()
  "Toggle the full hint docstring for the layouts transient-state."
  (interactive)
  (setq my--ts-full-hint-toggle
        (logxor my--ts-full-hint-toggle 1)))

(defun my//layout-format-name (name pos)
  "Format the layout name given by NAME for display in mode-line."
  (let* ((layout-name (if (file-directory-p name)
                          (file-name-nondirectory (directory-file-name name))
                        name))
         (string-name (format "%s" layout-name))
         (current (equal name (my//current-layout-name)))
         (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                          ":" string-name)))
    (if current
        (propertize (concat "[" caption "]") 'face 'warning)
      caption)))

(defun my//layouts-ts-hint ()
  "Return a one liner string containing all the layout names."
  (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                         (list persp-nil-name)))
         (formatted-persp-list
          (concat " "
                  (mapconcat (lambda (persp)
                               (my//layout-format-name
                                persp (position persp persp-list)))
                             persp-list " | "))))
    (concat
     formatted-persp-list
     (if (equal 1 my--ts-full-hint-toggle)
         my--layouts-ts-full-hint
       (concat "  (["
               (propertize "?" 'face 'hydra-face-red)
               "] help)")))))

(defun my/layout-switch-by-pos (pos)
  "Switch to perspective of position POS."
  (let ((persp-to-switch
         (nth pos (persp-names-current-frame-fast-ordered))))
    (if persp-to-switch
        (persp-switch persp-to-switch)
      (when (y-or-n-p
             (concat "Perspective in this position doesn't exist.\n"
                     "Do you want to create one? "))
        (let ((persp-reset-windows-on-nil-window-conf t))
          (persp-switch nil)
          (progn (my/new-empty-buffer)
                 (delete-other-windows)))))))

(dolist (i (number-sequence 9 0 -1))
  (eval `(defun ,(intern (format "my/persp-switch-to-%s" i)) nil
           ,(format "Switch to layout %s." i)
           (interactive)
           (my/layout-switch-by-pos ,(if (eq 0 i) 9 (1- i))))))

(defun my//current-layout-name ()
  "Get name of the current perspective."
  (safe-persp-name (get-frame-persp)))

(defun my//layout-autosave ()
  "Perspectives mode autosave.
Autosaves perspectives layouts every `persp-autosave-interal' seconds.
Cancels autosave on exiting perspectives mode."
  (if (and persp-mode layouts-enable-autosave)
      (progn
        (message "Perspectives mode autosaving enabled.")
        (setq my--layouts-autosave-timer
              (run-with-timer
               layouts-autosave-delay
               layouts-autosave-delay
               (lambda ()
                 (message "Saving perspectives to file.")
                 (persp-save-state-to-file)))))
    (when my--layouts-autosave-timer
      (cancel-timer my--layouts-autosave-timer)
      (setq my--layouts-autosave-timer nil))))

(defun my/jump-to-last-layout ()
  "Open the previously selected layout, if it exists."
  (interactive)
  (unless (eq 'non-existent
              (gethash my--last-selected-layout
                       *persp-hash* 'non-existent))
    (persp-switch my--last-selected-layout)))

(defun my/alternate-buffer-in-persp ()
  "Switch back and forth between current and last buffer in the
current perspective."
  (interactive)
  (with-persp-buffer-list ()
                          (switch-to-buffer (other-buffer (current-buffer) t))))

(defun my/layout-goto-default ()
  "Go to `Default' layout"
  (interactive)
  (persp-switch "Default"))

(defun my//custom-layout-func-name (name)
  "Return the name of the custom-perspective function for NAME."
  (intern (concat "my/custom-perspective-" name)))

(defmacro my|define-custom-layout (name &rest props)
  "Define a custom-perspective called NAME.

FUNC is a FUNCTION defined using NAME and the result of
`my//custom-layout-func-name', it takes care of
creating the perspective NAME and executing the expressions given
in the :body property to this macro.

NAME is a STRING.

Available PROPS:

`:binding STRING'
   Key to be bound to the function FUNC

`:body EXPRESSIONS'
  One or several EXPRESSIONS that are going to be evaluated after
  we change into the perspective NAME."
  (declare (indent 1))
  (let* ((name (if (symbolp name)
                   (symbol-value name)
                 name))
         (func (my//custom-layout-func-name name))
         (binding-prop (car (my/mplist-get props :binding)))
         (binding (if (symbolp binding-prop)
                      (symbol-value binding-prop)
                    binding-prop))
         (body (my/mplist-get props :body))
         (already-defined? (cdr (assoc binding
                                       my--custom-layout-alist))))
    `(progn
       (defun ,func ()
         ,(format "Open custom perspective %s" name)
         (interactive)
         (let ((initialize (not (gethash ,name *persp-hash*))))
           (persp-switch ,name)
           (when initialize
             (delete-other-windows)
             ,@body)))
       ;; Check for Clashes
       (if ,already-defined?
           (unless (equal ,already-defined? ,name)
             (my/warning "Replacing existing binding \"%s\" for %s with %s"
                                ,binding ,already-defined? ,name)
             (push '(,binding . ,name) my--custom-layout-alist))
         (push '(,binding . ,name) my--custom-layout-alist)))))

(defun my//custom-layouts-ms-documentation ()
  "Return the docstring for the custom perspectives transient-state."
  (if my--custom-layout-alist
      (mapconcat (lambda (custom-persp)
                   (format "[%s] %s"
                           (car custom-persp) (cdr custom-persp)))
                 my--custom-layout-alist " ")
    (my/warning (format "`my--custom-layout-alist' variable is empty" ))))

(defun my//update-custom-layouts ()
  "Ensure the custom-perspectives transient-state is updated.
Takes each element in the list `my--custom-layout-alist'
format so they are supported by the
`my/custom-layouts-transient-state' macro."
  (let (bindings)
    (dolist (custom-persp my--custom-layout-alist bindings)
      (let* ((binding (car custom-persp))
             (name (cdr custom-persp))
             (func-name (my//custom-layout-func-name name)))
        (push (list binding func-name :exit t) bindings)))
    (eval `(my|define-transient-state custom-layouts
             :doc (concat (my//custom-layouts-ms-documentation))
             :bindings
             ,@bindings))))

(defun my/select-custom-layout ()
  "Update the custom-perspectives transient-state and then activate it."
  (interactive)
  (my//update-custom-layouts)
  (my/custom-layouts-transient-state/body))

(defun my/layouts-ts-rename ()
  "Rename a layout and get back to the perspectives transient-state."
  (interactive)
  (call-interactively 'persp-rename)
  (my/layouts-transient-state/body))

(defun my/layouts-ts-close ()
  "Kill current perspective"
  (interactive)
  (persp-kill-without-buffers (my//current-layout-name)))

(defun my/layouts-ts-close-other ()
  (interactive)
  (call-interactively 'my/helm-persp-close)
  (my/layouts-transient-state/body))

(defun my/layouts-ts-kill ()
  "Kill current perspective"
  (interactive)
  (persp-kill (my//current-layout-name)))

(defun my/layouts-ts-kill-other ()
  (interactive)
  (call-interactively 'my/helm-persp-kill)
  (my/layouts-transient-state/body))


(defun my//basic/ui/layouts/init ()
  (setq eyebrowse-wrap-around t)
  (eyebrowse-mode)
  ;; transient state
  (my|transient-state-format-hint workspaces
                                  my--workspaces-ts-full-hint
        "\n\n
 Go to^^^^^^                         Actions^^
 ─────^^^^^^───────────────────────  ───────^^──────────────────────
 [_0_,_9_]^^     nth/new workspace   [_d_] close current workspace
 [_C-0_,_C-9_]^^ nth/new workspace   [_R_] rename current workspace
 [_<tab>_]^^^^   last workspace      [_?_] toggle help\n
 [_l_]^^^^       layouts
 [_n_/_C-l_]^^   next workspace
 [_N_/_p_/_C-h_] prev workspace\n")

  (my|define-transient-state workspaces
                             :title "Workspaces Transient State"
                             :hint-is-doc t
                             :dynamic-hint (my//workspaces-ts-hint)
                             :bindings
                             ("?" my//workspaces-ts-toggle-hint)
                             ("0" eyebrowse-switch-to-window-config-0 :exit t)
                             ("1" eyebrowse-switch-to-window-config-1 :exit t)
                             ("2" eyebrowse-switch-to-window-config-2 :exit t)
                             ("3" eyebrowse-switch-to-window-config-3 :exit t)
                             ("4" eyebrowse-switch-to-window-config-4 :exit t)
                             ("5" eyebrowse-switch-to-window-config-5 :exit t)
                             ("6" eyebrowse-switch-to-window-config-6 :exit t)
                             ("7" eyebrowse-switch-to-window-config-7 :exit t)
                             ("8" eyebrowse-switch-to-window-config-8 :exit t)
                             ("9" eyebrowse-switch-to-window-config-9 :exit t)
                             ("C-0" eyebrowse-switch-to-window-config-0)
                             ("C-1" eyebrowse-switch-to-window-config-1)
                             ("C-2" eyebrowse-switch-to-window-config-2)
                             ("C-3" eyebrowse-switch-to-window-config-3)
                             ("C-4" eyebrowse-switch-to-window-config-4)
                             ("C-5" eyebrowse-switch-to-window-config-5)
                             ("C-6" eyebrowse-switch-to-window-config-6)
                             ("C-7" eyebrowse-switch-to-window-config-7)
                             ("C-8" eyebrowse-switch-to-window-config-8)
                             ("C-9" eyebrowse-switch-to-window-config-9)
                             ("<tab>" eyebrowse-last-window-config)
                             ("C-h" eyebrowse-prev-window-config)
                             ("C-i" eyebrowse-last-window-config)
                             ("C-l" eyebrowse-next-window-config)
                             ("d" eyebrowse-close-window-config)
                             ("l" my/layouts-transient-state/body :exit t)
                             ("n" eyebrowse-next-window-config)
                             ("N" eyebrowse-prev-window-config)
                             ("p" eyebrowse-prev-window-config)
                             ("R" my/workspaces-ts-rename :exit t)
                             ("w" eyebrowse-switch-to-window-config :exit t))
  ;; note: we don't need to declare the `SPC l w' binding, it is
  ;; declare in the layout transient state
  (my/set-leader-keys "bW" 'my/goto-buffer-workspace)
  ;; hooks
  (add-hook 'persp-before-switch-functions
            #'my/update-eyebrowse-for-perspective)
  (add-hook 'eyebrowse-post-window-switch-hook
            #'my/save-eyebrowse-for-perspective)
  (add-hook 'persp-activated-functions
            #'my/load-eyebrowse-for-perspective)
      ;; vim-style tab switching
  (define-key evil-motion-state-map "gt" 'eyebrowse-next-window-config)
  (define-key evil-motion-state-map "gT" 'eyebrowse-prev-window-config)

  (my/set-leader-keys "pl" 'my/helm-persp-switch-project)

  (my|hide-lighter persp-mode)
  (setq persp-auto-resume-time (if dotfile-auto-resume-layouts 1 -1)
        persp-nil-name "Default"
        persp-reset-windows-on-nil-window-conf nil
        persp-set-last-persp-for-new-frames nil
        persp-save-dir my-layouts-directory)
  (my/defer-until-after-config #'my//activate-persp-mode)

  (my|transient-state-format-hint layouts
                                  my--layouts-ts-full-hint
        "\n\n
 Go to^^^^^^                                  Actions^^
 ─────^^^^^^──────────────────────────────    ───────^^──────────────────────────────────────────────────
 [_0_,_9_]^^     nth/new layout               [_a_]^^   add buffer
 [_C-0_,_C-9_]^^ nth/new layout               [_A_]^^   add all from layout
 [_<tab>_]^^^^   last layout                  [_d_]^^   close current layout
 [_b_]^^^^       buffer in layout             [_D_]^^   close other layout
 [_h_]^^^^       default layout               [_r_]^^   remove current buffer
 [_l_]^^^^       layout w/helm/ivy            [_R_]^^   rename current layout
 [_L_]^^^^       layouts in file              [_s_/_S_] save all layouts/save by names
 [_n_/_C-l_]^^   next layout                  [_t_]^^   show a buffer without adding it to current layout
 [_N_/_p_/_C-h_] prev layout                  [_x_]^^   kill current w/buffers
 [_o_]^^^^       custom layout                [_X_]^^   kill other w/buffers
 [_w_]^^^^       workspaces transient state   [_?_]^^   toggle help\n")

      (my|define-transient-state layouts
        :title "Layouts Transient State"
        :hint-is-doc t
        :dynamic-hint (my//layouts-ts-hint)
        :bindings
        ;; need to exit in case number doesn't exist
        ("?" my//layouts-ts-toggle-hint)
        ("1" my/persp-switch-to-1 :exit t)
        ("2" my/persp-switch-to-2 :exit t)
        ("3" my/persp-switch-to-3 :exit t)
        ("4" my/persp-switch-to-4 :exit t)
        ("5" my/persp-switch-to-5 :exit t)
        ("6" my/persp-switch-to-6 :exit t)
        ("7" my/persp-switch-to-7 :exit t)
        ("8" my/persp-switch-to-8 :exit t)
        ("9" my/persp-switch-to-9 :exit t)
        ("0" my/persp-switch-to-0 :exit t)
        ("C-1" my/persp-switch-to-1)
        ("C-2" my/persp-switch-to-2)
        ("C-3" my/persp-switch-to-3)
        ("C-4" my/persp-switch-to-4)
        ("C-5" my/persp-switch-to-5)
        ("C-6" my/persp-switch-to-6)
        ("C-7" my/persp-switch-to-7)
        ("C-8" my/persp-switch-to-8)
        ("C-9" my/persp-switch-to-9)
        ("C-0" my/persp-switch-to-0)
        ("<tab>" my/jump-to-last-layout)
        ("<return>" nil :exit t)
        ("C-h" persp-prev)
        ("C-l" persp-next)
        ("a" persp-add-buffer :exit t)
        ("A" persp-import-buffers :exit t)
        ("b" my/persp-helm-mini :exit t)
        ("d" my/layouts-ts-close)
        ("D" my/layouts-ts-close-other :exit t)
        ("h" my/layout-goto-default :exit t)
        ("l" my/helm-perspectives :exit t)
        ("L" persp-load-state-from-file :exit t)
        ("n" persp-next)
        ("N" persp-prev)
        ("o" my/select-custom-layout :exit t)
        ("p" persp-prev)
        ("r" persp-remove-buffer :exit t)
        ("R" my/layouts-ts-rename :exit t)
        ("s" persp-save-state-to-file :exit t)
        ("S" persp-save-to-file-by-names :exit t)
        ("t" persp-temporarily-display-buffer :exit t)
        ("w" my/workspaces-transient-state/body :exit t)
        ("x" my/layouts-ts-kill)
        ("X" my/layouts-ts-kill-other :exit t))

      (my/set-leader-keys "l" 'my/layouts-transient-state/body)
      ;; custom layouts
      (my|define-custom-layout "@Emacs"
                               :binding "e"
                               :body
                               (my/find-dotfile))

      (defadvice persp-activate (before my//save-toggle-layout activate)
        (setq my--last-selected-layout persp-last-persp-name))
      (add-hook 'persp-mode-hook 'my//layout-autosave)
      (my/declare-prefix "b" "persp-buffers")
      (my/declare-prefix "B" "global-buffers")
      ;; Override SPC TAB to only change buffers in perspective
      (my/set-leader-keys
       "TAB"  'my/alternate-buffer-in-persp
       "ba"   'persp-add-buffer
       "br"   'persp-remove-buffer
       "Bb"   'my-layouts/non-restricted-buffer-list))

(provide 'my//basic/ui/layouts)
