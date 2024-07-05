;; -*- lexical-binding: t; -*-

(csetq
  (use-package-always-defer t)
  (use-package-use-theme nil))

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
(add-to-list-after 'use-package-keywords :extra-modes :hooks)

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

(defun use-package-normalize/:extra-modes (name-symbol keyword args)
  (let ((label (symbol-name keyword))
        (wrong-syntax (lambda ()
                        (use-package-error
                         (format "%s whats (<mode> <mode>...) or list of these"
                                 label)))))
    (when (null args)
      (use-package-error (format "%s wants a non-empty list" label)))
    (let ((results (list)))
      (dolist (arg args)
        (pcase arg
          (`(,mode . ,modes)
           (push `(,mode . ,modes) results))
          (_ (funcall wrong-syntax))))
      (reverse results))))

;; if x-mode is a minor mode
;;  - on enable:
;;    - add 'x-mode to local-minor-modes/global-minor-modes
;;    - run x-mode-hook
;;  - on disable:
;;    - remove 'x-mode from local-minor-modes/global-minor-modes
;;    - run x-mode-hook

;; if x-mode is a major mode
;;  - on enable:
;;    - set major-mode to 'x-mode
;;    - run x-mode-hook
;;  - on disable:
;;    - set major-mode to 'x-mode
;;    - run change-major-mode-hook

(defun my--extra-mode-on-mode-hook (extra-mode-fn mode)
  (funcall extra-mode-fn (if (or (memq mode local-minor-modes)
                                 (memq mode global-minor-modes)
                                 (derived-mode-p mode))
                             1
                           -1)))

(defun my--extra-mode-on-major-mode-change-hook (extra-mode-fn mode)
  (when (derived-mode-p mode)
    (funcall extra-mode-fn -1)))

(defun use-package-handler/:extra-modes (name-symbol _keyword args rest state)
  (let ((elems (mapcar (lambda (mode/modes)
                         (pcase mode/modes
                           (`(,mode . ,modes)
                            (let ((hook (intern (concat (symbol-name mode)
                                                        "-hook")))
                                  (exprs-1 (mapcar (lambda (extra-mode)
                                                     `(lambda ()
                                                        (my--extra-mode-on-mode-hook #',extra-mode ',mode)))
                                                   modes))
                                  (exprs-2 (mapcar (lambda (extra-mode)
                                                     `(lambda ()
                                                        (my--extra-mode-on-major-mode-change-hook #',extra-mode ',mode)))
                                                   modes)))
                              `(progn
                                 (my-add-hooks ',hook (list ,@exprs-1))
                                 (my-add-hooks 'change-major-mode-hook (list ,@exprs-2)))))))
                       args))
        (rest-elems (use-package-process-keywords name-symbol rest state)))
    (use-package-concat elems rest-elems)))

(provide 'core-use-package)
