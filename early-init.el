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

;;;; Setup native compilation --------------------------------------------------

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  ;; TODO: For some reason this make emacs recompile all the installed
  ;; packages on every startup...
  ;; (push (expand-file-name "eln-cache/" my-cache-dir) native-comp-eln-load-path)
  (setq-default
   native-comp-async-report-warnings-errors nil
   native-comp-deferred-compilation t
   native-comp-jit-compilation t))

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

;; Native compile packages when they're installed.
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (setq package-native-compile t))
