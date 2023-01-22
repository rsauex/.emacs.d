;; -*- lexical-binding: t; -*-

(use-package marginalia
  :ensure t
  :custom
  (marginalia-align 'right)
  :init
  (marginalia-mode 1))

(use-package vertico
  :ensure t
  :custom
  (suggest-key-bindings nil)
  (vertico-resize nil)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Better directory navigation
  :bind ((:map vertico-map)
         ("RET"   . vertico-directory-enter)
         ("DEL"   . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hooks
  ;; Tidy shadowed file names
  (rfn-eshadow-update-overlay vertico-directory-tidy)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-setup-hook cursor-intangible-mode)
  :init
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  (vertico-mouse-mode 1)

  (defvar my-vertico-transform-functions nil
    "A list of function to transfrom completion candidates.")

  (defun my-vertico--highlight-directory (file)
    "Highlight FILE if it ends with a slash."
    (if (string-suffix-p "/" file)
        (propertize file 'face 'marginalia-file-priv-dir)
      file))

  (defun my-vertico--sort-directories-first (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-history-length-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  :config
  ;; Align marginalia in `execute-extended-command' so it's easier to read
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command (marginalia-align . left)))

  ;; Make files list look more like in Ivy
  (add-to-list 'vertico-multiform-categories
               '(file (vertico-sort-function . my-vertico--sort-directories-first)
                      (my-vertico-transform-functions . (my-vertico--highlight-directory))))

  (define-advice vertico--format-candidate
      (:filter-args (args) my-vertico-transform)
    "Apply functions from `my-vertico-transform-functions' before
formatting the candidate."
    (dolist (fun (ensure-list my-vertico-transform-functions) args)
      (setcar args (funcall fun (car args)))))

  (define-advice completing-read-multiple
      (:filter-args (args) add-cmr-indicator)
    "Add prompt indicator to `completing-read-multiple'.
We display [CRM<separator>], e.g., [CRM,] if the separator is a comma."
    (pcase args
      (`(,prompt . ,rest-args)
       (cons (format "[CRM%s] %s"
                     (replace-regexp-in-string
                      "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                      crm-separator)
                     prompt)
             rest-args)))))

(use-package consult
  :ensure t
  :bind (;; ("C-s" . consult-line)
         (:map minibuffer-local-map)
         ("C-r" . consult-history)))

(provide 'core-vertico)
