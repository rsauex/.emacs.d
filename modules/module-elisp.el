;; -*- lexical-binding: t; -*-

(require 'module-paredit)
(require 'module-aggressive-indent)

(use-package emacs-lisp-mode
  :hooks
  (eval-expression-minibuffer-setup-hook . (enable-paredit-mode))
  :extra-modes
  (emacs-lisp . (paredit-mode
                 aggressive-indent-mode))
  (ielm-mode . (paredit-mode)))

(provide 'module-elisp)
