;; Directories
(defconst my-emacs-dir
  (expand-file-name user-emacs-directory))

(defconst my-core-dir
  (concat my-emacs-dir "core/"))

;; (defconst my-site-lisp-dir
;;   (expand-file-name "site-lisp" user-emacs-directory))

(defconst my-settings-dir
  (concat my-emacs-dir "settings/"))

(defconst my-modules-dir
  (concat my-emacs-dir "modules/"))

(defconst my-cache-dir
  (concat my-emacs-dir ".cache/"))

(defconst my-backup-dir
  (concat my-cache-dir "backups/"))

;;;;

;; Encoding
(set-charset-priority 'unicode)
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(setq-default
 enable-recursive-minibuffers nil
 auto-save-list-file-prefix (concat my-cache-dir "auto-save-list/.saves-")
 server-auth-dir (concat my-cache-dir "server/")
 
 thumbs-thumbsdir (concat my-cache-dir "thumbs/")
 ;; mpc-data-directory (concat my-cache-dir "mpc/")

 tramp-persistency-file-name (concat my-cache-dir "tramp")

 ;; image-dired-dir (concat my-cache-dir "image-dired/")
 ;; image-dired-db-file (concat my-cache-dir "image-dired/.image-dired_db")
 ;; image-dired-gallery-dir (concat my-cache-dir "image-dired/.image-dired_gallery")

 recentf-save-file (concat my-cache-dir "recentf")

 savehist-file (concat my-cache-dir "history"))

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,my-backup-dir))
      delete-old-versions  t
      kept-new-versions  6
      kept-old-versions  2
      version-control  t)

;; Save point position between sessions
(save-place-mode 1)
(setq-default save-place-file (concat my-cache-dir "places"))

;; Set up load path
(add-to-list 'load-path my-core-dir)
(add-to-list 'load-path my-settings-dir)
(add-to-list 'load-path my-modules-dir)

;; Major mode for scratch buffer
(setq initial-major-mode 'text-mode)

;; No key suggesings
(setq suggest-key-bindings nil)

;;; Package setup
(load (concat user-emacs-directory "core/core-package.el"))

(eval-when-compile
  (require 'core-use-package))

(use-package diminish :ensure t)
(use-package bind-key :ensure t)

(defun my-add-hooks (hook functions)
  (dolist (fn (reverse functions))
    (add-hook hook fn)))

;; Undo Tree mode
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  ;; Keep region when undoing in region
  (defadvice undo-tree-undo (around keep-region activate)
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          ad-do-it
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      ad-do-it)))

;; Uniquify buffer names
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package core-appearance)
(use-package core-sane-defaults)
(use-package core-vc)
(use-package core-dired)
(use-package core-ivy)
(use-package core-editing)

(provide 'core)
