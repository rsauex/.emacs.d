;; -*- lexical-binding: t; -*-

(define-advice window-toggle-side-windows
    (:before (&rest _) select-mru-window)
  "Select most recently used window when closing side windows"
  (select-window (get-mru-window)))

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
