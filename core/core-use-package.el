;; -*- lexical-binding: t; -*-

(unless (require 'use-package nil 'noerror)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'cl-lib)

(defun add-to-list-after (list-var thing after-thing)
  (let ((old-value (remove thing (symbol-value list-var))))
    (unless (member after-thing old-value)
      (error "%s not found in %s" after-thing list-var))
    (let* ((pivot-position (1+ (cl-position after-thing old-value)))
           (before (butlast old-value (- (length old-value) pivot-position)))
           (after (nthcdr pivot-position old-value)))
      (set list-var (append before (list thing) after))))
  (symbol-value list-var))

(add-to-list-after 'use-package-keywords :hooks :hook)

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

(add-to-list-after 'use-package-keywords :custom-local :hooks)

(defun use-package-normalize/:custom-local (_name keyword args)
  (let* ((label (symbol-name keyword))
         (wrong-syntax (lambda ()
                         (use-package-error
                          (concat label
                                  " a (<mode> (<symbol> <value>)...)"
                                  "  or list of these")))))
    (when (null args)
      (use-package-error (concat label " wants a non-empty list")))
    (let ((make-hook-sym (lambda (sym)
                           (intern (concat (symbol-name sym) "-hook"))))
          (results (list)))
      (dolist (arg args)
        (pcase arg
          (`(,mode . ,vars/vals)
           (dolist (var/val vars/vals)
             (pcase var/val
               (`(,var ,val)
                (push `(,(funcall make-hook-sym mode) ,var ,val) results))
               (_ (funcall wrong-syntax)))))
          (_ (funcall wrong-syntax))))
      (reverse results))))

(defun use-package-handler/:custom-local (name _keyword args rest state)
  (let ((elems (mapcar (lambda (hook/var/value)
                         (pcase hook/var/value
                           (`(,hook ,var ,value)
                            `(add-hook ',hook (lambda () (setq-local ,var ,value))))))
                       args))
        (rest-elems (use-package-process-keywords name rest state)))
    (use-package-concat elems rest-elems)))

(provide 'core-use-package)
