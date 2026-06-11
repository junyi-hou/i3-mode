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
  "The window manager flavor.  Supports `sway', `i3', and `aerospace'."
  :type 'symbol
  :group 'i3)

;;;###autoload
(define-minor-mode i3-mode
  "Delegate the window management role to i3wm."
  :global t
  :group 'i3
  :lighter nil
  (if i3-mode
      (progn
        (i3--check-executables)
        (add-hook 'server-after-make-frame-hook #'i3--check-sway-environment-variables))
    (remove-hook 'server-after-make-frame-hook #'i3--check-sway-environment-variables)))

(defun i3--check-executables ()
  "Check whether dependency is satisfied."
  (let* ((dep (pcase i3-flavor
                ('sway '("swaymsg" "jq" "sway-call"))
                ('aerospace '("aerospace" "jq" "aerospace-call"))
                (_ '("xprop" "i3-msg" "i3-call"))))
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
  "Run ARGS with `i3-msg', `swaymsg', or `aerospace' based on `i3-flavor'.
ARGS is a list of strings.  Returns nil.

For `aerospace', `focus' and `move' commands are translated to use
the --direction flag: (\"focus\" \"right\") => aerospace focus --direction right."
  (cl-letf (((symbol-function 'message) #'ignore))
    (let* ((cmd (pcase i3-flavor
                  ('sway "swaymsg")
                  ('aerospace "aerospace")
                  (_ "i3-msg")))
           (translated-args
            (if (eq i3-flavor 'aerospace)
                (pcase args
                  (`(,(and c (or "focus" "move")) ,dir . ,rest)
                   `(,c "--direction" ,dir ,@rest))
                  (_ args))
              args)))
      (make-process :name (car args)
                    :command `(,cmd ,@translated-args)
                    :coding 'utf-8
                    :buffer i3--debug-buffer)
      nil)))



;;;###autoload
(defun i3-integrated-key (keysym &rest i3-command)
  "Pass KEYSYM to the current buffer.  If there is a command associated with
KEYSYM, run the command interactively.  Otherwise call `i3-msg' with
I3-COMMAND.

Prefix commands are handled via `this-command-keys': e.g. if \"C-l\" is bound
in i3 but the user types \"C-c C-l\", the stored prefix \"C-c\" is prepended
before the binding lookup.

The target buffer is determined by `frame-selected-window' of `selected-frame',
which reflects the window currently displayed in the frame rather than relying
on buffer recency order."
  (let* ((prefixes (this-command-keys))
         (keysym (kbd keysym))
         (key-sequence (concat prefixes keysym))
         (buf (window-buffer (frame-selected-window (selected-frame)))))
    (switch-to-buffer buf)
    (if (commandp (key-binding key-sequence))
        (condition-case _
            (progn
              (call-interactively (key-binding key-sequence))
              (unless (equal (this-command-keys) "")
                (setq unread-command-events (listify-key-sequence "\C-g"))))
          (error (apply #'i3-msg i3-command))))))


;;; installation

;; install script to ~/.local/bin/
(cl-eval-when 'compile
  (let* ((repo (file-name-as-directory (expand-file-name "")))
         (scripts '("i3-call" "sway-call" "aerospace-call"))
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
