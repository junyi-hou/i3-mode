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
        (add-hook 'kill-emacs-hook #'i3-revert-config)
        (add-hook 'server-after-make-frame-hook #'i3--check-sway-environment-variables))

    (i3-revert-config)
    (remove-hook 'kill-emacs-hook #'i3-revert-config)
    (remove-hook 'server-after-make-frame-hook #'i3--check-sway-environment-variables)))

(defun i3--check-executables ()
  "Check whether dependency is satisfied."
  (let* ((dep (if (eq i3-flavor 'sway) '("swaymsg" "jq" "sway-call") '("xprop" "i3-msg" "i3-call")))
         (pass (eval `(and ,@(mapcar 'executable-find dep)))))
    (unless pass
      (user-error "Necessary executable not found, please make sure %s are in your `exec-path'" (s-join ", " dep)))))

(defun i3--check-sway-environment-variables ()
  "Check if SWAYSOCK is set.  If not, set it."
  (when (and (eq i3-flavor 'sway)
             (not (seq-find (lambda (str) (string-match-p "^SWAYSOCK" str)) process-environment)))
    (let* ((pid (string-to-number (shell-command-to-string "pidof sway")))
           (uid (user-real-uid))
           (sway-sock (format "/run/user/%d/sway-ipc.%d.%d.sock" uid uid pid)))
      (unless (= 0 pid)
        (add-to-list 'process-environment (format "SWAYSOCK=%s" sway-sock))))))


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

(defun i3-update-config ()
  "Update `i3-config-file' to include the new key bindings defined in `i3-bindings'."
  (let ((i3-backup-file (format "%s.backup" i3-config-file)))
    (f-move i3-config-file i3-backup-file)
    (with-temp-buffer
      (insert-file i3-backup-file)
      (goto-char (point-max))
      (dolist (config i3-extra-config)
        (if (functionp config)
            (insert (funcall config))
          (insert config)))
      (write-file (expand-file-name i3-config-file)))
    (i3-msg "reload")))

(defun i3-revert-config ()
  "Remove the focus move bindings from the i3 config."
  (f-delete i3-config-file)
  (f-move (format "%s.backup" i3-config-file) i3-config-file)
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
get captured by the window manager, which will call `emacsclient' and run this command.
As a result, this command will be ran in *server* buffer. A hack I use to fix this is to
notice two observations:

1. if called without \"-c\" switch, `emacsclient' will use `selected-frame'.
2. the current buffer sits on top (`car') of the `buffer-list'.

Therefore, I can first switch to the current buffer, then pass the event \"C-l\" to it."
  (let* ((prefixes (kbd (key-description (this-command-keys-vector))))
         (keysym (kbd keysym))
         (key-sequence (concat prefixes keysym))
         (buf (car (buffer-list (selected-frame)))))
    (switch-to-buffer buf)
    (or (ignore-error (call-interactively (key-binding key-sequence)) (keyboard-quit))
        (apply #'i3-msg i3-command))))


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
