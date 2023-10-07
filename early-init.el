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
