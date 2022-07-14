;; -*- lexical-binding: t -*-

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

;;;; Directories ---------------------------------------------------------------

(defconst my-emacs-dir
  (expand-file-name user-emacs-directory))

(defconst my-core-dir
  (expand-file-name "core/" my-emacs-dir))

(defconst my-settings-dir
  (expand-file-name "settings/" my-emacs-dir))

(defconst my-modules-dir
  (expand-file-name "modules/" my-emacs-dir))

(defconst my-cache-dir
  (expand-file-name ".cache/" my-emacs-dir))

(defconst my-backup-dir
  (expand-file-name "backups/" my-cache-dir))

(defconst my-package-dir
  (expand-file-name "elpa/" my-cache-dir))

;; Force all packages to create files in cache dir...
(setq user-emacs-directory my-cache-dir)

;;;; Set up load path ----------------------------------------------------------

(add-to-list 'load-path my-core-dir)
(add-to-list 'load-path my-settings-dir)
(add-to-list 'load-path my-modules-dir)

;;;; Simple customize-set-variable ---------------------------------------------

(defmacro csetq (&rest specs)
  "Assign variables correctly handling customs.

\(fn [VAR VALUE [COMMENT]]...)"
  (declare (indent 0))
  (let ((%spec (lambda (spec)
                 (pcase spec
                   (`(,variable ,value)
                    `(customize-set-variable ',variable ,value))
                   (`(,variable ,value ,comment)
                    `(customize-set-variable ',variable ,value ,comment))
                   (_ (error "Invalid `csetq' binding: %s" spec))))))
    `(progn
       ,@(mapcar %spec specs)
       nil)))

;;;; Encoding ------------------------------------------------------------------

(set-language-environment 'utf-8)
(csetq (default-input-method nil)) ;; `set-language-environment' sets `default-input-method', which is unwanted

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
  (savehist-mode 1)
  (history-length 50)
  (history-delete-duplicates t)
  (savehist-additional-variables (list 'search-ring
                                       'regexp-search-ring)))

;;;; Save point position between sessions --------------------------------------

(use-package saveplace
  :custom
  (save-place-mode 1))

;;;; Uniquify buffer names -----------------------------------------------------

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

;;;; Tramp ---------------------------------------------------------------------

(use-package tramp
  :defer t
  :custom
  (tramp-default-method "ssh"))

;;;; Autorevert ----------------------------------------------------------------

(use-package autorevert
  :custom
  (global-auto-revert-mode 1)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

;;;; Other core packages -------------------------------------------------------

(require 'core-fonts)
(require 'core-appearance)
(require 'core-dired)
(require 'core-ivy)
(require 'core-editing)
(require 'core-package)
(require 'core-company)
(require 'core-project)

(provide 'core)
