;; -*- lexical-binding: t; -*-

(require 'package)

;; Force reevalute package dirs because their values were computed
;; before early-init.el had a chance to set user-emacs-directory.
(dolist (var '(package-user-dir
               package-gnupghome-dir))
  (custom-reevaluate-setting var))

(csetq
  (package-archives `(,@package-archives
                      ("melpa" . "https://melpa.org/packages/")))
  (package-install-upgrade-built-in t))

(package-initialize)

(unless (file-exists-p (expand-file-name "archives" package-user-dir))
  (package-refresh-contents))

(provide 'core-package)
