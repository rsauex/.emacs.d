;; -*- lexical-binding: t; -*-

(require 'module-paredit)
(require 'module-aggressive-indent)

(use-package slime
  :ensure t
  :hooks
  (slime-repl-mode-hook . ((lambda () (define-key slime-repl-mode-map
                                        (read-kbd-macro paredit-backward-delete-key) nil))))
  :extra-modes
  (slime . (paredit-mode
            aggressive-indent-mode))
  (slime-repl . (paredit-mode))
  :custom
  (slime-load-failed-fasl 'never)
  (slime-net-coding-system 'utf-8-unix)
  (inferior-lisp-program "sbcl --dynamic-space-size 5120")
  :config (progn
            (slime-setup '(slime-asdf
                           slime-fancy
                           slime-indentation
                           slime-sprof
                           ;; slime-snapshot
                           slime-tramp
                           slime-xref-browser))))

(use-package lispy
  :ensure t
  :custom
  (lispy-colon-p nil))

(use-package lisp-mode
  :extra-modes
  (lisp-mode . (paredit-mode
                slime-mode
                lispy-mode))
  (lisp-data-mode . (paredit-mode
                     lispy-mode)))

(provide 'module-cl)
