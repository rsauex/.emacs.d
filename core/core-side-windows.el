;; -*- lexical-binding: t; -*-

(add-to-list 'window-persistent-parameters
             '(no-delete-other-windows . writable))
(add-to-list 'window-persistent-parameters
             '(no-other-window . writable))

(add-to-list 'window-persistent-parameters
             '(window-side-selected . writable))

(define-advice window-toggle-side-windows
    (:before (&optional frame) select-mru-window)
  "Select most recently used window when hiding side windows"
  (when (window-with-parameter 'window-side nil (window-normalize-frame frame))
    (when (window-parameter (selected-window) 'window-side)
      (set-window-parameter (selected-window) 'window-side-selected t))
    (select-window (get-mru-window))))

(define-advice window-toggle-side-windows
    (:after (&optional frame) maybe-select-side-window)
  "Select previously selected side window if there was one when
showing side windows"
  (let ((frame (window-normalize-frame frame)))
    (when (window-with-parameter 'window-side nil frame)
      (when-let ((window-to-select (window-with-parameter 'window-side-selected nil frame)))
        (select-window window-to-select))
      (dolist (window (window-list frame))
        (set-window-parameter window 'window-side-selected nil)))))

;; TODO: better key
(bind-key "C-z" #'window-toggle-side-windows)

(defconst my-default-side-window-parameters
  '((side . bottom)
    (slot . 1)
    (preserve-size . (nil . t))
    (window-parameters . ((no-other-window . t)
                          (no-delete-other-windows . t)))))

(defun add-side-windows-rule (criterion &rest args)
  (let ((entry `(display-buffer-in-side-window
                 ,@args
                 ,@my-default-side-window-parameters)))
    (setf (alist-get criterion display-buffer-alist nil nil #'equal) entry)
    nil))

(add-side-windows-rule "^\\*")

(provide 'core-side-windows)
