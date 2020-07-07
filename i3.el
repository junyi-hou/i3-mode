;;; i3.el --- i3 compatibility mode -*- lexical-binding: t; -*-

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; Maintainer: Junyi Hou <junyi.yi.hou@gmail.com>
;; Version: 0.0.1
;; Package-requires: ((emacs "27") (dash "2.17.0") (s "1.12.0"))

;;; code

(require 'dash)
(require 's)
(require 'cl-lib)

(defgroup i3 nil
  "Customization group for `i3-mode'"
  :group 'convenience
  :prefix "i3-")

(defcustom i3-function-should-split-window
  '(magit-display-buffer transient--show)
  "A list of function where `i3-split-window' will use the original `split-window'.
This means commands that call any function in this list will split window within an emacs frame, whereas commands that does not call these functions will split frame in two x-windows.

list of functions:
`magit-display-buffer' - so all magit calls split window instead of frame
`transient--show' - The current implementation moves frame focus. So as soon as transient creates a new frame, the frame will got kill immediately due to focus out. Put `transient--show' here will instruct transient to use window split which retains focus and allow transient to do its job."
  :type '(repeat function)
  :group 'i3)

(defcustom i3-config-file "~/.config/i3/config"
  "Path to the i3 config file."
  :type 'string
  :group 'i3)

(defcustom i3-bindings
  '((?\C-l . "focus right")
    (?\C-h . "focus left")
    (?\C-k . "focus up")
    (?\C-j . "focus down"))
  "Key bindings that should be effective both in i3 and emacs.
A typical use case is to allow same set of key to move focus both across windows within the same emacs frame and across x-windows."
  :type '(repeat list)
  :group 'i3)

;;;###autoload
(define-minor-mode i3-mode
  "Delegate the window management role to i3wm"
  :global t
  :group 'i3
  :lighter nil
  (if i3-mode
      (progn
        (advice-add #'split-window :around #'i3-split-window)
        (advice-add #'delete-window :around #'i3-delete-window)
        (advice-add #'pop-to-buffer :around #'i3-pop-to-buffer)

        (i3-update-config))
    (advice-remove #'split-window #'i3-split-window)
    (advice-remove #'delete-window #'i3-delete-window)
    (advice-remove #'pop-to-buffer #'i3-pop-to-buffer)

    (i3-revert-config)))


;; general IPC

(defvar i3--debug-buffer " *i3-debug*")

;;;###autoload
(defun i3-msg (&rest args)
  "Run commands string with `i3-msg'.
ARGS takes the form of list of string.
examples:
(i3-msg '((\"resize\" \"set\" \"200\"))) => i3-msg resize set 200"
  (cl-letf (((symbol-function 'message) #'ignore))
    (make-process :name (car args)
                  :command `("i3-msg" ,@args)
                  :buffer i3--debug-buffer)))


;;; update i3 config

(defvar i3--config nil
  "Internal variable save the unmodified i3 config")

(defun i3-update-config ()
  "Update i3 config from `i3-config-file'.
This function makes two changes to the i3 config file. First, it replaces the colorscheme of i3 and i3 bars (if any) to the current emacs scheme. Second, it adds the key binding defined in `i3-bindings'.  The old config is save to `i3--config', which can be reverted by invoking `i3-revert-config'."
  ;; first save current config
  (setq i3--config (with-temp-buffer
                     (insert-file-contents (expand-file-name i3-config-file))
                     (buffer-string)))

  (with-temp-buffer
    (-> i3--config
        i3--bar-color          ; update i3bar colors
        i3--window-color       ; update window colors
        i3--key-binding-config ; update key-bindings
        insert
        )
    (write-file (expand-file-name i3-config-file)))

  (i3-msg "reload"))

(defun i3-revert-config ()
  "Remove the focus move bindings from the i3 config."
  (with-temp-buffer
    (insert i3--config)
    (write-file (expand-file-name i3-config-file)))
  (i3-msg "reload"))

(defun i3--bar-color (str)
  "Update the color scheme of i3bar to the current theme, where STR is the config file stored in one big string.  Return new config file string."
  ;; 1. get current setting
  (let ((bar-config '())
        bar-config-start-pos
        bar-config-end-pos)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "^bar" nil 'noerror)
        (setq bar-config-start-pos (line-beginning-position))
        (setq bar-config-end-pos (progn (re-search-forward "^}" nil)
                                        (line-end-position)))
        (add-to-list
         'bar-config
         (-> (buffer-substring-no-properties
              bar-config-start-pos
              bar-config-end-pos)
             (split-string "\n")))
        (delete-region bar-config-start-pos
                       bar-config-end-pos))

      ;; 2 update bar color config
      (let* ((background (face-attribute 'default :background))
             (foreground (face-attribute 'default :foreground))
             (f-in-bg (face-attribute 'mode-line :background))
             (f-in-fg (face-attribute 'mode-line :foreground))
             (f-out-bg (face-attribute 'mode-line-inactive :background))
             (f-out-fg (face-attribute 'mode-line-inactive :foreground))
             (ug-bg (face-attribute 'font-lock-warning-face :background))
             (ug-fg (face-attribute 'font-lock-warning-face :foreground)))

        (->> bar-config
             (--map
              `(,@(-butlast it)
                ,@(backquote
                   (,(concat (s-repeat 8 " ") "colors {")
                    ,(concat (s-repeat 16 " ") "background " background)
                    ,(concat (s-repeat 16 " ") "statusline " foreground)
                    ,(concat (s-repeat 16 " ") "focused_workspace " f-in-bg " " f-in-bg " " f-in-fg)
                    ,(concat (s-repeat 16 " ") "active_workspace " f-out-bg " " f-out-bg " " f-out-fg)
                    ,(concat (s-repeat 16 " ") "inactive_workspace " f-out-bg " " f-out-bg " " f-out-fg)
                    ,(concat (s-repeat 16 " ") "urgent_workspace " ug-bg " " ug-bg " " ug-fg)
                    ,(concat (s-repeat 8 " ") "}\n")))
                ,(concat (-last-item it) "\n")))
             (--map (s-join "\n" it))
             (mapc 'insert)))
      ;; 3 return new config string
      (buffer-string))))

(defun i3--window-color (str)
  "Update the color scheme of i3 window client to the current theme, where STR is the config file stored in one big string.  Return new config file string."
  (let ((window-color nil))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "^client\\.\\([a-z_]+\\)" nil 'noerror)
        (cond ((string= (match-string 1) "focused")
               (add-to-list
                'window-color
                (concat "client.focused "
                        (face-attribute 'mode-line :background) " " ; border
                        (face-attribute 'default :background) " "   ; backgr
                        (face-attribute 'mode-line :foreground) " "   ; text
                        (face-attribute 'default :foreground) " "   ; indicator
                        (face-attribute 'mode-line :background) "\n"))) ; childborder
              ((member (match-string 1) '("unfocused" "focused_inactive"))
               (add-to-list
                'window-color
                (concat
                 "client." (match-string 1) " "
                 (face-attribute 'mode-line-inactive :background) " " ; border
                 (face-attribute 'default :background) " "   ; backgr
                 (face-attribute 'mode-line-inactive :foreground) " " ; text
                 (face-attribute 'mode-line-inactive :foreground) " " ; indicator
                 (face-attribute 'mode-line-inactive :background) "\n"))) ;childborder
              ((string= (match-string 1) "urgent")
               (add-to-list
                'window-color
                (concat
                 "client.urgent "
                 (face-attribute 'font-lock-warning-face :foreground) " " ; border
                 (face-attribute 'default :background) " "   ; backgr
                 (face-attribute 'mode-line :foreground) " " ; text
                 (face-attribute 'mode-line :foreground) " " ; indicator
                 (face-attribute 'mode-line-inactive :background) "\n"))) ;childborder
              )
        (delete-region (line-beginning-position) (line-end-position)))
      (mapc 'insert `("\n" ,@window-color))
      (buffer-string))))

(defun i3--key-binding-config (str)
  "Append i3 config STR with key bindings according to `i3-bindings'.  Return the updated string."
  (with-temp-buffer
    (insert str)
    (let ((key-binding-string "\n"))
      (dolist (binding i3-bindings)
        (let* ((mod (--> (car binding)
                         (event-modifiers it)
                         (-map 'symbol-name it)
                         (-map 's-capitalize it)
                         (s-join "+" it)))
               (key (-> (car binding)
                        event-basic-type vector (key-description nil)))
               (cmd (concat "i3-call "
                            (cdr binding) " "
                            (-> (car binding) vector key-description))))
          (setq key-binding-string
                (concat key-binding-string
                        "bindsym " mod "+" key " exec --no-startup-id " cmd "\n"))))
      (insert key-binding-string))
    (buffer-string)))

;;;###autoload
(defun i3-integrated-key (keysym &rest i3-command)
  "Pass KEYSYM to the current buffer, if there is a command associated with KEYSYM, run the command interactively.  Otherwise call `i3-msg' with I3-COMMAND.

Note that This function needs to consider prefix command. e.g., if one binds \"C-l\" to move focus to the right xwindow in i3wm, then prefixed \"C-l\" like \"C-c C-l\" also needs to get to emacs. In order to solve this problem, one must 1. make i3wm aware of the prefix state of emacs, and 2. find the appropriate command associated to the key sequence to run *in the appropriate buffer*.

See also `i3-call' shell script for how to handle prefix commands in the shell process."
  (let* ((prefixes (kbd (key-description (this-command-keys-vector))))
         (keysym (kbd keysym)))
    (condition-case _
        (call-interactively (key-binding (or (and (string= prefix "") keysym)
                                             (concat prefix keysym))))
      (error (apply #'i3-msg i3-command)))))


;;; replacing window management with i3


;; 1. split window

(defconst i3-split-axis
  '((v . "height")
    (h . "width")))

;;;###autoload
(defun i3-split-window (fn &optional window size side pixelwise)
  (let ((callers (i3--call-stack))
        (-compare-fn #'eq))
    (if (-intersection callers i3-function-should-split-window)
        (funcall fn window size side pixelwise)
      (i3--split-window window size side pixelwise))))

(defun i3--split-window (&optional window size side pixelwise)
  "Overriding `split-window' to use frames instead of window."
  (let* ((window (or window (selected-window)))
         (side (cond
		            ((not side) 'below)
                ((eq side 'up) 'above)
                ((eq side 'down) 'below)
		            ((memq side '(below above right left)) side)
		            (t 'right)))
         (how (if (memq side '(below above)) 'v 'h))
         (axis (alist-get how i3-split-axis))
         (total-pixel-size (funcall (intern (concat "frame-pixel-" axis))))
         (old-window-size (or
                           ;; case 1: use pixel-wise size, then size is the final size
                           (and pixelwise size (if (> size 0) size
                                                 (+ total-pixel-size size)))
                           ;; case 2: given size, but in terms of line/columns
                           ;; approximate using default-font-AXIS * SIZE
                           (and size (if-let* ((unit (funcall
                                                      (intern (concat "default-font-" axis))))
                                               ((> size 0)))
                                         (* unit size)
                                       (+ total-pixel-size (* unit size))))
                           ;; case 3: size is not given, set it to -1
                           -1)))
    (i3--switch-to-frame (window-frame window))
    (i3-msg "split" (symbol-name how))
    (make-process :name "emacs-client"
                  :command `("emacsclient" "-c" "-n" "-e"
                             "(switch-to-buffer (car (buffer-list)))")
                  :buffer i3--debug-buffer)

    ;; now move the focus to mimic split at different directions
    ;; and return the new window
    ;; FIXME: use the new emacsclient to call the following code?
    ;; but then how to return windows?
    (sleep-for 0.2) ;; HACK: need to allow some time for emacsclient to finish loading
    (let ((new-window nil))
      (cond ( ; new window is below - move focus up
             (eq side 'below) (progn
                                (setq new-window (selected-window))
                                (i3-msg "focus" "up")))
            ( ; new window is right - move focus left
             (eq side 'right) (progn
                                (setq new-window (selected-window))
                                (i3-msg "focus" "left")))
            ;; now new window is above/left, which means the original window
            ;; is the below/right window. We need to move focus back and forth
            (t (setq new-window (car (window-list (next-frame))))))
      (when (> old-window-size 0)
        (i3-msg "resize" "set" axis (int-to-string old-window-size)))
      ;; return the new window
      new-window)))

(defun i3--switch-to-frame (frame-or-window-id)
  "Switch the focus to frame identified by FRAME-OR-WINDOW-ID."
  (let ((window-id (or (and (frame-live-p frame-or-window-id)
                            (frame-parameter frame-or-window-id 'outer-window-id))
                       frame-or-window-id)))
    (i3-msg (concat "[id=\"" window-id "\"]") "focus")))

(defun i3--call-stack ()
  "Return the list of function called.
  example:
  (defun bar () (i3--call-stack))
  (defun foo () (bar))

  (foo) ;; returns (command-execute call-interactively funcall-interactively eval-last-sexp elisp--eval-last-sexp eval bar foo)"
  (let ((frames)
        (frame)
        (index 5))
    (while (setq frame (backtrace-frame index))
      (push frame frames)
      (cl-incf index))
    (mapcar 'cadr (cl-remove-if-not 'car frames))))


;; 2. delete-window

;;;###autoload
(defun i3-delete-window (fn &optional window)
  "Allow `delete-window' to delete frame which WINDOW lives in if there WINDOW is the only window in that frame."
  (let ((frame (if (and window (windowp window))
                   (window-frame window)
                 (selected-frame))))
    (if (= 1 (length (window-list frame)))
        (delete-frame frame)
      (funcall fn window))))


;; 3. pop-to-buffer

;;;###autoload
(defun i3-pop-to-buffer (fn buffer-or-name &optional action norecord)
  "If the frame containing BUFFER-OR-NAME is visible, goto that frame instead.  Otherwise call normal `pop-to-buffer'."
  (let ((window (get-buffer-window buffer-or-name 'all-frames)))
    (if window
        (-> window
            window-frame
            i3--switch-to-frame)
      (funcall fn buffer-or-name action norecord))))



;;; other utilities

(defvar i3--launcher-debug-buffer " *i3-launcher*")

;;;###autoload
(defun i3-launcher (&optional completion-fn)
  "Use a emacsclient and the provided completion framework to launch programs.
To use this function, set keybindings in i3 config by using
bindsym KEY exec --no-startup-id emacsclient -c -e \"(i3-launcher)\""
  ;; make current window floating
  (i3-msg (concat "[id=\"" (frame-parameter (selected-frame) 'outer-window-id) "\"]")
          "floating" "enable")
  (set-frame-size (selected-frame) 50 12)

  ;; switch to a blank buffer
  (switch-to-buffer "*scratch*")

  (let* ((completion-fn (or completion-fn #'completing-read))
         (program (funcall completion-fn "run:"
                           (->> exec-path
                                (--map (directory-files it nil "^[^._]"))
                                -flatten
                                delete-dups)))
         (listified-program (s-split " " program)))
    (make-process :name (car listified-program)
                  :command listified-program
                  :buffer i3--launcher-debug-buffer)
    (delete-frame)))


;;; installation

;; put script to user-emacs-directory/bin/
(cl-eval-when 'compile
  (let* ((repo (file-name-as-directory (expand-file-name "")))
         (script-name (expand-file-name "i3-call" repo))
         (target (expand-file-name "bin/i3-call" user-emacs-directory)))
    (unless (file-exists-p (expand-file-name "bin" user-emacs-directory))
      (make-directory (expand-file-name "bin" user-emacs-directory)))
    (when (file-exists-p target) (delete-file target))
    (make-symbolic-link script-name target nil)))


(provide 'i3)
;;; i3.el ends here
