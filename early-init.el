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

;; Store customize settings in a separate file
(setq custom-file (expand-file-name "custom.el" my-emacs-dir))

;;;; Set up load path ----------------------------------------------------------

(add-to-list 'load-path my-core-dir)
(add-to-list 'load-path my-modules-dir)

;;;; Setup GUI early -----------------------------------------------------------

(setq-default
  menu-bar-mode nil
  tool-bar-mode nil
  scroll-bar-mode nil
  use-dialog-box nil)

(add-to-list 'default-frame-alist '(font . "Monospace-12"))

;;;; Setup Package early -------------------------------------------------------

;; Don't init package system automatically because it will be inited
;; manually once it's preperly configured.
(setq package-enable-at-startup nil)
