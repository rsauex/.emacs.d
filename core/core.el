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

(defconst my-modules-dir
  (expand-file-name "modules/" my-emacs-dir))

(defconst my-cache-dir
  (expand-file-name ".cache/" my-emacs-dir))

;; Force all packages to create files in cache dir...
(setq user-emacs-directory my-cache-dir)

;;;; Set up load path ----------------------------------------------------------

(add-to-list 'load-path my-core-dir)
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

(csetq
  (default-input-method nil) ;; `current-language-environment' sets `default-input-method', which is unwanted
  (current-language-environment 'utf-8))

;;;; Backups -------------------------------------------------------------------

(csetq
  (backup-directory-alist `(("." . ,(expand-file-name "backup/" my-cache-dir))))
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

;;;; Autosave ------------------------------------------------------------------

(csetq
  (auto-save-list-file-prefix (concat user-emacs-directory "auto-save-list/.saves-")))

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
  :custom
  (tramp-default-method "ssh"))

;;;; Autorevert ----------------------------------------------------------------

(use-package autorevert
  :custom
  (global-auto-revert-mode 1)
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil))

;;;; Ace Window ----------------------------------------------------------------

(use-package posframe
  :ensure t)

(use-package ace-window
  :ensure t
  :bind (("M-o". ace-window))
  :custom
  ;; Behave the same way regardless of the number of windows
  (aw-dispatch-always t)
  ;; Use postframe to display leading chars
  (ace-window-posframe-mode t)
  :custom-face
  ;; Use a much bigger font to display leading chars
  (aw-leading-char-face ((t (:height 5.0))))

  :init
  (defun my/ace-window-in-direction-or-error (direction)
    "Makes ace-window to use a window relative to the currect window
or throws an error when no window in that direction exists."
    (let ((window (window-in-direction direction (selected-window) t)))
      (unless window
        (if (or (eq direction 'left)
                (eq direction 'right))
            (error "No window to the %s" direction)
          (error "No window %s" direction)))
      (throw 'done (cons ?_ window))))

  (defun my/ace-window-left ()
    (my/ace-window-in-direction-or-error 'left))

  (defun my/ace-window-right ()
    (my/ace-window-in-direction-or-error 'right))

  (defun my/ace-window-below ()
    (my/ace-window-in-direction-or-error 'below))

  (defun my/ace-window-above ()
    (my/ace-window-in-direction-or-error 'above))

  :config
  ;; Use `htns' keys for windows relative to the current windows
  (setf (alist-get ?h aw-dispatch-alist) (list #'my/ace-window-left)
        (alist-get ?t aw-dispatch-alist) (list #'my/ace-window-below)
        (alist-get ?n aw-dispatch-alist) (list #'my/ace-window-above)
        (alist-get ?s aw-dispatch-alist) (list #'my/ace-window-right)))

;;;; Other core packages -------------------------------------------------------

(require 'core-fonts)
(require 'core-appearance)
(require 'core-dired)
(require 'core-orderless)
(require 'core-vertico)
(require 'core-ivy)
(require 'core-editing)
(require 'core-corfu)
(require 'core-project)
(require 'core-side-windows)

;;;; Custom --------------------------------------------------------------------

(setq custom-file (expand-file-name "custom.el" my-emacs-dir))
(load custom-file)

(provide 'core)
