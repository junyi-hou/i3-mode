;;; i3-mode.el --- i3 compatibility mode -*- lexical-binding: t; -*-

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; Maintainer: Junyi Hou <junyi.yi.hou@gmail.com>
;; Version: 0.0.1
;; Package-requires: ((emacs "27") (dash "2.17.0") (s "1.12.0"))

;;; code

(require 'dash)
(require 's)
(require 'f)
(require 'cl-lib)
(require 'server)

(defgroup i3 nil
  "Customization group for `i3-mode'"
  :group 'convenience
  :prefix "i3-")

(defcustom i3-flavor 'sway
  "The window manager flavor, now supports `sway' and `i3'."
  :type 'symbol
  :group 'i3)

(defcustom i3-config-file (expand-file-name (format ".config/%s/config" (symbol-name i3-flavor)) (getenv "HOME"))
  "Path to the i3 config file."
  :type 'string
  :group 'i3)

(defcustom i3-extra-config nil
  "A list of extra settings that should be append to the end of `i3-config-file'. Each element can be either a string or a function with no argument and returns a string."
  :type 'list
  :group 'i3)

;;;###autoload
(define-minor-mode i3-mode
  "Delegate the window management role to i3wm"
  :global t
  :group 'i3
  :lighter nil

  (if i3-mode
      (progn
        (i3--check-executables)
        (i3-update-config)
        (add-hook 'kill-emacs-hook #'i3-revert-config))

    (i3-revert-config)
    (remove-hook 'kill-emacs-hook #'i3-revert-config)))

(defun i3--check-executables ()
  "Check whether dependency is satisfied."
  (let* ((dep (if (eq i3-flavor 'sway) '("swaymsg" "jq" "sway-call") '("xprop" "i3-msg" "i3-call")))
         (pass (eval `(and ,@(mapcar 'executable-find dep)))))
    (unless pass
      (user-error "exectable not found, please make sure %s are in your `exec-path'" (s-join ", " dep)))))


;; general IPC

(defvar i3--debug-buffer " *i3-debug*")

;;;###autoload
(defun i3-msg (&rest args)
  "Run commands string with `i3-msg' or `swaymsg' depend on the value of `i3-flavor'.
ARGS takes the form of list of string.

examples:
(i3-msg '(\"resize\" \"set\" \"200\")) => i3-msg resize set 200"
  (cl-letf (((symbol-function 'message) #'ignore)
            (cmd (if (eq i3-flavor 'sway) "swaymsg" "i3-msg")))
    (make-process :name (car args)
                  :command `(,cmd ,@args)
                  :buffer i3--debug-buffer)))


;;; manipulate config file

(defvar i3--config nil
  "A cons cell whose car is the path to the symlink origin of the `i3-config-file' if it is a symlink, otherwise `nil'. The cdr of this variable is the content of `i3-config-file'.")

(defun i3--save-original-config ()
  "Return a con cell (file-symlink-origin . file-content) of the original `i3-config-file'.

See docstring of `i3--config' for more information."
  (cons (and (f-symlink-p i3-config-file) (file-chase-links i3-config-file 1))
        (with-temp-buffer (insert-file-contents i3-config-file) (buffer-string))))

(defun i3-update-config ()
  "Update `i3-config-file' to include the new key bindings defined in `i3-bindings'."
  ;; first save current config
  (setq i3--config (i3--save-original-config))
  (f-delete i3-config-file)
  (with-temp-buffer
    (insert (cdr i3--config))
    (dolist (config i3-extra-config)
      (if (functionp config)
          (insert (funcall config))
        (insert config)))
    (write-file (expand-file-name i3-config-file)))
  (i3-msg "reload"))

(defun i3-revert-config ()
  "Remove the focus move bindings from the i3 config."
  (f-delete i3-config-file)
  (if (car i3--config)
      (f-symlink (car i3--config) i3-config-file)
    (with-temp-buffer
      (insert (cdr i3--config))
      (write-file i3-config-file)))
  (setq i3--config nil)
  (i3-msg "reload"))

;;;###autoload
(defun i3-integrated-key (keysym &rest i3-command)
  "Pass KEYSYM to the current buffer.  If there is a command associated with
KEYSYM, run the command interactively.  Otherwise call `i3-msg' with
I3-COMMAND.

Note that This function needs to consider prefix command.  e.g., if one binds
\"C-l\" to move focus to the right xwindow in i3wm, then prefixed \"C-l\" like
\"C-c C-l\" also needs to get to Emacs.  This can be done by using
`this-command-keys-vector' function.

The second, and more tricky issue is that the \"C-l\" will need to get to the
appropriate buffer.  This is not obvious, as when we press \"C-l\", it will first
get captured by the window manager (meaning emacs is potentially losing focus
now).  I use a hack that manually set the buffer as the `car' of `buffer-list'
in the `selected-frame'."
  (let* ((prefixes (kbd (key-description (this-command-keys-vector))))
         (keysym (kbd keysym))
         ;; HACK: I forget why this works...
         (buf (car (buffer-list (selected-frame)))))
    (condition-case _
        (progn
          (switch-to-buffer buf)
          (call-interactively (key-binding (or (and (string= prefixes "") keysym)
                                               (concat prefixes keysym))))
          (keyboard-quit))
      (error (apply #'i3-msg i3-command)))))


;;; installation

;; install script to ~/.local/bin/
(cl-eval-when 'compile
  (let* ((repo (file-name-as-directory (expand-file-name "")))
         (scripts '("i3-call" "sway-call"))
         (target-directory (expand-file-name ".local/bin/" (getenv "HOME"))))
    (unless (file-exists-p target-directory)
      (make-directory target-directory))

    (dolist (script scripts)
      (let ((target (format "%s%s" target-directory script)))
        (when (f-exists-p target)
          (delete-file target))
        (f-symlink (expand-file-name script) target)))))

(provide 'i3-mode)
;;; i3-mode.el ends here
