;; -*- lexical-binding: t; -*-

(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles my-orderless-file basic))))
  :init
  (require 'cl-extra)

  (defvar original-orderless-component-separator nil
    "Keeps the original value of `orderless-component-separator'
when `my-orderless-separate-path' is used.")

  (defun my-orderless-remote-prefix (component index _total)
    "If the first component looks like a method part of tramp path,
force it to be an anchored quoted regexp so that it's appended to
the completion prefix."
    (when (and (= 0 index) (string-match-p "\\`[^/|:]+:$" component))
      `(orderless-regexp . ,(rx-to-string `(seq bol ,component)))))

  (defun my-orderless-separate-path (pattern)
    "Split tramp method from path pattern into a separate compontent
and split the rest of the pattern using the original `orderless-component-separator'."
    (let ((path (substitute-in-file-name pattern))
          (prefix nil))
      (save-match-data
        (when (string-match "\\`[^/|:]+:" path)
          (setq prefix (match-string 0 path))
          (setq path (substring path (match-end 0))))
        (let ((components (if (functionp original-orderless-component-separator)
                              (funcall original-orderless-component-separator path)
                            (split-string path original-orderless-component-separator t))))
          (if prefix
              (cons prefix components)
            components)))))

  :config
  (orderless-define-completion-style my-orderless-file
    "Orderless completion style which handles tramp methods correctly."
    (original-orderless-component-separator orderless-component-separator)
    (orderless-component-separator #'my-orderless-separate-path)
    (orderless-style-dispatchers `(my-orderless-remote-prefix ,@orderless-style-dispatchers)))

  (define-advice tramp-completion-handle-file-name-all-completions
      (:filter-return (result) tramp-should-obey-completion-regexp-list)
    "Tramp disregards `completion-regexp-list' when completing methods
and hosts for whatever reason. This advice filters the results."
    (let ((case-fold-search completion-ignore-case))
      (seq-filter (lambda (candidate)
                    (cl-every (lambda (regexp)
                                (string-match-p regexp candidate))
                              completion-regexp-list))
                  result))))

(provide 'core-orderless)
