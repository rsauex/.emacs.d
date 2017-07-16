(use-package module-paredit)
(use-package module-company)

(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)

(use-package emacs-lisp-mode
  :defer t
  :hooks ((emacs-lisp-mode-hook . (enable-paredit-mode company-mode))
          (ielm-mode-hook . (enable-paredit-mode company-mode))))

(provide 'module-elisp)
