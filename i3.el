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
  )


;;; replacing window management with i3

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

(defconst i3-split-axis
  '((v . "height")
    (h . "width")))

(defun i3--switch-to-frame (frame)
  "Switch the focus to FRAME.  FRAME must be visible and live."
  (unless (and frame (frame-live-p frame) (frame-visible-p frame))
    (user-error "FRAME must be live and visible"))
  (let ((window-id (- (string-to-number (frame-parameter frame 'window-id)) 4)))
    (i3-cmd (concat "[id=\"" (int-to-string window-id) "\"]") "focus")))

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

(defcustom i3-function-should-use-window
  '(magit-display-buffer transient--show)
  "A list of function where `i3-split-window' will use the original `split-window'."
  :type '(repeat function)
  :group 'i3)

(defun i3-split-window (fn &optional window size side pixelwise)
  (let ((callers (i3--call-stack)))
    (if (cl-intersection callers i3-function-should-use-window)
        (funcall fn window size side pixelwise)
      (i3--split-window window size side pixelwise))))


(defun i3--split-window (&optional window size side pixelwise)
  "Overriding `split-window' to use frames instead of window."
  (let* ((side (cond
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
                  :command `("emacsclient" "-c" "-n")
                  :buffer i3--debug-buffer)

    ;; now move the focus to mimic split at different directions
    ;; and return the new window
    (sleep-for 0.1) ;; HACK: need to allow some time for emacsclient to finish loading
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

;; (advice-add 'split-window :override 'i3--split-window)
(advice-add 'split-window :around #'i3-split-window)


(defun i3-move-focus (direction)
  "Move focus in DIRECTION.  When error, move focus using i3's focus move mechanism."
  (interactive)
  (let* ((direction (symbol-name direction))
         (fn (intern (concat "windmove-" direction))))
    (condition-case _
        (funcall fn)
      (error (i3-msg "focus" direction)))))


(provide 'i3)
;;; i3.el ends here
