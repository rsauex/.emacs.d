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

(defconst my-gc-cons-threshold-suspended
  most-positive-fixnum)

(defconst my-gc-cons-threshold-normal
  (* 16 1024 1024))

;; Functions to suspend/resume GC

(defun my--suspend-gc ()
  (setq gc-cons-threshold my-gc-cons-threshold-suspended))

(defun my--resume-gc ()
  (setq gc-cons-threshold my-gc-cons-threshold-normal))

;; Suspend GC while in minibuffer

(add-hook 'minibuffer-setup-hook #'my--suspend-gc)
(add-hook 'minibuffer-exit-hook #'my--resume-gc)

;; Suspend GC during startup

(my--suspend-gc)
(add-hook 'emacs-startup-hook 'my--resume-gc)

;;;; Simple customize-set-variable ---------------------------------------------

(require 'cl-lib)

(defmacro csetq (&rest specs)
  "Assign variables correctly handling customs.

\(fn [VAR VALUE [COMMENT]]...)"
  (declare (indent 0))
  `(progn
     ,@(mapcar (lambda (spec)
                 (cl-destructuring-bind (variable value &optional comment)
                     spec
                   (if comment
                       `(customize-set-variable ',variable ,value ,comment)
                     `(customize-set-variable ',variable ,value))))
               specs)
     nil))

;;;; Encoding ------------------------------------------------------------------

(set-language-environment 'utf-8)
(csetq (default-input-method nil)) ;; `set-language-environment' sets `default-input-method', which is unwanted

;;;; Folders & files -----------------------------------------------------------

(csetq
  (server-auth-dir             (concat my-cache-dir "server/"))
  (thumbs-thumbsdir            (concat my-cache-dir "thumbs/"))
  (recentf-save-file           (concat my-cache-dir "recentf"))
  (kkc-init-file-name          (concat my-cache-dir "kkcrc")))

;;;; Auto save -----------------------------------------------------------------

(csetq
  (auto-save-list-file-prefix (concat my-cache-dir "auto-save-list/.saves-")))

;;;; Bookmarks -----------------------------------------------------------------

(csetq
  (bookmark-default-file (concat my-cache-dir "bookmarks"))
  (bookmark-save-flag t))

;;;; Backups -------------------------------------------------------------------

(csetq
  (backup-directory-alist `(("." . ,my-backup-dir)))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t))

;;;; Package setup -------------------------------------------------------------

(require 'core-package)

(eval-when-compile
  (require 'core-use-package))

(use-package diminish :ensure t)
(use-package bind-key :ensure t)

(defun my-add-hooks (hook functions)
  (dolist (fn (reverse functions))
    (add-hook hook fn)))

;;;; History -------------------------------------------------------------------

(use-package savehist
  :custom
  (history-length 50)
  (history-delete-duplicates t)
  (savehist-file (concat my-cache-dir "history"))
  (savehist-additional-variables (list 'search-ring
                                       'regexp-search-ring))
  :config
  (savehist-mode 1))

;;;; Save point position between sessions --------------------------------------

(use-package saveplace
  :custom
  (save-place-file (concat my-cache-dir "places"))
  :config
  (save-place-mode 1))

;;;; Uniquify buffer names -----------------------------------------------------

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

;;;; Common packages -----------------------------------------------------------

(use-package s
  :ensure t)

;;;; Tramp ---------------------------------------------------------------------

(use-package tramp
  :defer t
  :custom
  (tramp-default-method "ssh")
  (tramp-persistency-file-name (concat my-cache-dir "tramp")))

;;;; Abbrev --------------------------------------------------------------------

(use-package abbrev
  :defer t
  :custom
  (abbrev-file-name (concat my-cache-dir "abbrev_defs")))

;;;; Other core packages -------------------------------------------------------

(require 'core-fonts)
(require 'core-appearance)
(require 'core-sane-defaults)
(require 'core-dired)
(require 'core-ivy)
(require 'core-editing)
(require 'core-package)
(require 'core-company)
(require 'core-project)

(provide 'core)
