;; -*- lexical-binding: t; -*-

(csetq
  (use-package-always-defer t))

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

;; Use my own keyword for hooks
(add-to-list-after 'use-package-keywords :hooks :hook)
(setq use-package-keywords (remove :hook use-package-keywords))

(defun use-package-normalize/:hooks (name-symbol keyword args)
  (let ((label (symbol-name keyword))
        (wrong-syntax (lambda ()
                        (use-package-error
                         (format "%s whats (<hook> <expr>...) or list of these"
                                 label)))))
    (when (null args)
      (use-package-error (format "%s wants a non-empty list" label)))
    (let ((results (list)))
      (dolist (arg args)
        (pcase arg
          (`(,hook . ,exprs)
           (push `(,hook . ,exprs) results))
          (_ (funcall wrong-syntax))))
      (reverse results))))

(defun use-package-handler/:hooks (name-symbol _keyword args rest state)
  (let ((elems (mapcar (lambda (hook/exprs)
                         (pcase hook/exprs
                           (`(,hook . ,exprs)
                            `(my-add-hooks ',hook '(,@exprs)))))
                       args))
        (rest-elems (use-package-process-keywords name-symbol rest state)))
    (use-package-concat elems rest-elems)))

(add-to-list-after 'use-package-keywords :custom-local :hooks)

(defun use-package-normalize/:custom-local (_name keyword args)
  (let* ((label (symbol-name keyword))
         (wrong-syntax (lambda ()
                         (use-package-error
                          (format "%s whats (<mode> (<symbol> <value>)...) or list of these"
                                  label)))))
    (when (null args)
      (use-package-error (format "%s wants a non-empty list" label)))
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
