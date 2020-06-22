;;; i3.el --- i3 compatibility mode -*- lexical-binding: t; -*-


(defgroup i3 nil
  "Customization group for `i3-mode'"
  :group 'convenience
  :prefix "i3-")

(defcustom i3-config-file-location '("~/.config/i3/config" "/etc/i3/config")
  "A list of possible locations for i3 config file. If given a string, it should be the location of the i3 config file. If given a list, then going down that list until finds a i3 config file."
  :type '(choose string (repeat string))
  :group 'i3)


;;; replacing window management with i3

(defun i3-cmd (cmd)
  "Run CMD string with `i3-msg'."
  (shell-command (concat "i3-msg " cmd)))

(defun i3--split-vertical (&optional size)
  "Split current window vertically in 2.

If optional argument SIZE is omitted or nil, both windows get the
same height, or close to it.  If SIZE is positive, the upper
(selected) window gets SIZE lines.  If SIZE is negative, the
lower (new) window gets -SIZE lines."
  (i3-cmd "split vertical")
  (when size
    ))

(defun i3--split-horizontal ()
  "Split current window horizontally in 2"
  (i3-cmd "split horizontal"))

(defun i3--focus-right ()
  "Move focus to the right window"
  (interactive)
  (i3-cmd "focus right"))

(defun i3--focus-left ()
  "Move focus to the left window"
  (interactive)
  (i3-cmd "focus left"))

(defun i3--focus-up ()
  "Move focus to the up window"
  (interactive)
  (i3-cmd "focus up"))

(defun i3--focus-down ()
  "Move focus to the right window"
  (interactive)
  (i3-cmd "focus down"))


(provide 'i3)
;;; i3.el ends here
