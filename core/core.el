;; -*- lexical-binding: t -*-

;;;; Directories ---------------------------------------------------------------

(defconst my-emacs-dir
  (expand-file-name user-emacs-directory))

(defconst my-core-dir
  (expand-file-name "core/" my-emacs-dir))

(defconst my-settings-dir
  (expand-file-name "settings/" my-emacs-dir))

(defconst my-modules-dir
  (expand-file-name "modules/"  my-emacs-dir))

(defconst my-cache-dir
  (expand-file-name ".cache/" my-emacs-dir))

(defconst my-backup-dir
  (expand-file-name "backups/" my-cache-dir))

(defconst my-package-dir
  (expand-file-name "elpa/" my-cache-dir))

;;;; Set up load path ----------------------------------------------------------

(add-to-list 'load-path my-core-dir)
(add-to-list 'load-path my-settings-dir)
(add-to-list 'load-path my-modules-dir)

;;;; GC ------------------------------------------------------------------------

(let ((gc-cons-threshold-original gc-cons-threshold))
  (setq gc-cons-threshold (* 50 1000 1000))

  (defun my/reset-gc-threshold ()
    "Reset `gc-cons-threshold' to its default value."
    (setq gc-cons-threshold gc-cons-threshold-original))

  (add-hook 'emacs-startup-hook 'my/reset-gc-threshold)

  ;; No GC while in minibuffer
  (defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun my-minibuffer-exit-hook ()
    (setq gc-cons-threshold gc-cons-threshold-original))

  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook))

;;;; Encoding ------------------------------------------------------------------

(set-charset-priority 'unicode)
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;;;; Folders & files -----------------------------------------------------------

(setq-default
 auto-save-list-file-prefix  (concat my-cache-dir "auto-save-list/.saves-")
 server-auth-dir             (concat my-cache-dir "server/")
 thumbs-thumbsdir            (concat my-cache-dir "thumbs/")
 package-user-dir            (concat my-cache-dir "elpa/")
 tramp-persistency-file-name (concat my-cache-dir "tramp")
 recentf-save-file           (concat my-cache-dir "recentf")
 kkc-init-file-name          (concat my-cache-dir "kkcrc"))

;;;; History -------------------------------------------------------------------

(setq savehist-file (concat my-cache-dir "history"))
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(search-ring
        regexp-search-ring))

;;;; Bookmarks ----------------------------------------------------------------

(setq bookmark-default-file (concat my-cache-dir "bookmarks"))
(setq bookmark-save-flag t)

;;;; Backups -------------------------------------------------------------------

(setq backup-directory-alist `(("." . ,my-backup-dir))
      delete-old-versions  t
      kept-new-versions  6
      kept-old-versions  2
      version-control  t)

;;;; Save point position between sessions --------------------------------------

(setq-default save-place-file (concat my-cache-dir "places"))
(save-place-mode 1)

;;;; Package setup -------------------------------------------------------------

(require 'core-package)

(eval-when-compile
  (require 'core-use-package))

(use-package diminish :ensure t)
(use-package bind-key :ensure t)

(defun my-add-hooks (hook functions)
  (dolist (fn (reverse functions))
    (add-hook hook fn)))

;;;; Uniquify buffer names -----------------------------------------------------

(use-package uniquify
  :defer 2
  :config (setq uniquify-buffer-name-style 'forward))

;;;; Common packages -----------------------------------------------------------

(use-package s
  :ensure t)

;;;; Tramp ---------------------------------------------------------------------

(use-package tramp
  :init
  (setq tramp-default-method "ssh"))

;;;; Other core packages -------------------------------------------------------

(use-package core-fonts)
(use-package core-appearance)
(use-package core-sane-defaults)
(use-package core-vc)
(use-package core-dired)
(use-package core-ivy)
(use-package core-editing)
(use-package core-package)
(use-package core-company)

(provide 'core)
