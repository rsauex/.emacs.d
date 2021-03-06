(use-package module-paredit)

(setq-default inferior-lisp-program "sbcl --dynamic-space-size 5120")

(use-package slime-company
  :defer t
  :ensure t)

(use-package slime
  :ensure t
  :commands (slime slime-mode slime-connect)
  :hooks ((slime-mode-hook . (enable-paredit-mode))
          (slime-repl-mode-hook . (enable-paredit-mode
                                   (lambda () (define-key slime-repl-mode-map
                                                (read-kbd-macro paredit-backward-delete-key) nil)))))
  :config (progn
            (slime-setup '(slime-asdf
                           slime-fancy
                           slime-indentation
                           slime-company
                           slime-sprof
                           slime-snapshot
                           slime-tramp
                           slime-xref-browser))
            (setq-default slime-net-coding-system 'utf-8-unix)))

(use-package lispy
  :commands (lispy-mode)
  :ensure t)

(use-package lisp-mode
  :defer t
  :hooks ((lisp-mode-hook . (enable-paredit-mode
                             slime-mode
                             lispy-mode))))

(provide 'module-cl)
