;; -*- lexical-binding: t; -*-

(unless (require 'use-package nil 'noerror)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'use-package-keywords :hooks)

(defun use-package-normalize/:hooks (name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (cond
       ((not (listp arg)) (use-package-error ":hooks wants a list"))
       ((null arg) arg)
       ((consp (car arg)) arg)
       ((symbolp (car arg)) 
        (list (cons (make-symbol (concat (symbol-name name-symbol) "-hook")) arg)))
       (t
        (use-package-error
         ":pin wants an archive name (a string)"))))))

(defun use-package-handler/:hooks (name-symbol keyword archive-name rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    (if (null archive-name)
        body
      (use-package-concat
       (mapcar (lambda (hook-list)
                 (let ((hook (car hook-list))
                       (functions (cdr hook-list)))
                   `(my-add-hooks ',hook '(,@functions))))
               archive-name)
       body))))

(provide 'core-use-package)
