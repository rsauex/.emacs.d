(use-package module-paredit)
(use-package module-company)

(setq-default inferior-lisp-program "sbcl")

(use-package slime
  :ensure t
  :commands (slime slime-mode slime-connect)
  :hooks ((slime-mode-hook . (enable-paredit-mode company-mode))
          (slime-repl-mode-hook . (enable-paredit-mode
                                   company-mode
                                   (lambda () (define-key slime-repl-mode-map
                                                (read-kbd-macro paredit-backward-delete-key) nil)))))
  :config (progn
            (slime-setup '(slime-asdf
                           slime-fancy
                           slime-indentation
                           slime-company
                           slime-sprof
                           slime-snapshot))
            (setq-default slime-net-coding-system 'utf-8-unix)))

(use-package lisp-mode
  :defer t
  :hooks ((lisp-mode-hook . (enable-paredit-mode slime-mode company-mode))))

(provide 'module-cl)
