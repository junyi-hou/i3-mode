;;; i3.el --- i3 compatibility mode -*- lexical-binding: t; -*-

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;; Maintainer: Junyi Hou <junyi.yi.hou@gmail.com>
;; Version: 0.0.1
;; Package-requires: ((emacs "26"))


(defgroup i3 nil
  "Customization group for `i3-mode'"
  :group 'convenience
  :prefix "i3-")

;;;###autoload
(define-minor-mode i3-mode
  "Delegate the window management role to i3wm"
  :global t
  nil nil nil
  (if i3-mode
      (progn
        (advice-add #'split-window-below :override #'i3-split-vertical)
        (advice-add #'split-window-right :override #'i3-split-horizontal)
        (advice-add #'other-window :override #'other-frame))
    (advice-remove #'split-window-below #'i3-split-vertical)
    (advice-remove #'split-window-right #'i3-split-horizontal)
    (advice-remove #'other-window #'other-frame)))


;;; replacing window management with i3

(defun i3-cmd (&rest args)
  "Run CMD string with `i3-msg'."
  (cl-letf (((symbol-function 'message) #'ignore))
    (make-process :name "i3-cmd" :command `("i3-msg" ,@args))))

(defconst i3-split-axis
  '((v . "height")
    (h . "width")))

(defun i3--split (how &optional size)
  "Split current window in 2.
HOW determines whether it is a vertical or horizontal split.
If optional argument SIZE is omitted or nil, both windows get the
same height, or close to it.  If SIZE is positive, the upper
(selected) window gets SIZE lines.  If SIZE is negative, the
lower (new) window gets -SIZE lines."
  (apply #'i3-cmd `("split" ,(symbol-name how)))
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
    (make-process :name "client" :command '("emacsclient" "-c" "-n")))
  (sleep-for 0.01) ; otherwise i3 do not know which window to resize
  (when size
    (let* ((axis (alist-get how i3-split-axis))
           (total-pixel-size (funcall (intern (concat "frame-pixel-" axis))))
           (unit (funcall (intern (concat "default-font-" axis))))
           (pixel-size (* unit size))
           (final-size (if (> size 0)
                           (round (- total-pixel-size pixel-size))
                         (- 0 (round pixel-size)))))
      (i3-cmd "resize" "set" axis (int-to-string final-size)))))

(defun i3-split-vertical (&optional size)
  "Split current window vertically in two.
If optional argument SIZE is omitted or nil, both windows get the
same height, or close to it.  If SIZE is positive, the upper
(selected) window gets SIZE lines.  If SIZE is negative, the
lower (new) window gets -SIZE lines."
  (interactive)
  (i3--split 'v size))

(defun i3-split-horizontal (&optional size)
  "Split current window horizontally in two.
If optional argument SIZE is omitted or nil, both windows get the
same height, or close to it.  If SIZE is positive, the upper
(selected) window gets SIZE lines.  If SIZE is negative, the
lower (new) window gets -SIZE lines."
  (interactive)
  (i3--split 'h size))

(defun i3-move-focus (direction)
  "Move focus in DIRECTION.  When error, move focus using i3's focus move mechanism."
  (interactive)
  (let* ((direction (symbol-name direction))
         (fn (intern (concat "windmove-" direction))))
    (condition-case _
        (funcall fn)
      (error (i3-cmd "focus" direction)))))

(provide 'i3)
;;; i3.el ends here
