;; -*- lexical-binding: t -*-

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

(use-package buffer-name-relative
  :ensure t
  :custom
  (buffer-name-relative-mode 1)
  (buffer-name-relative-prefix '("<" . ">/"))
  (buffer-name-relative-root-functions (list 'buffer-name-relative-root-path-from-vc
                                             (cl-constantly "~/"))))

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
  :bind* (("M-o". ace-window))
  :custom
  ;; Behave the same way regardless of the number of windows
  (aw-dispatch-always t)
  ;; Use postframe to display leading chars
  (ace-window-posframe-mode (posframe-workable-p))
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

;;;; Popup Frames --------------------------------------------------------------

;; TODO: simplify/rewrite
;; TODO: move to a more appropriate place

(defmacro define-popup-frame-fns (name fns exit-fns)
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (fn)
                 `(defun ,(intern (concat (symbol-name fn) "-popup-frame")) (&rest args)
                    (set-frame-parameter nil 'my-window-popup-frame ',name)
                    (apply #'funcall-interactively #',fn args)))
               fns)
     ,@(mapcar (lambda (fn)
                 `(define-advice ,fn
                      (:after (&rest _args) ,(intern (concat (symbol-name name) "-delete-frame-if-popup")))
                    (when (eq ',name (frame-parameter nil 'my-window-popup-frame))
                      (delete-frame))))
               exit-fns)))

(define-popup-frame-fns calc
  (full-calc)
  (calc-quit))

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

(provide 'core)
