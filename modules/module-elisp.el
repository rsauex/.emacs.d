(use-package module-paredit)

(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)

(use-package emacs-lisp-mode
  :defer t
  :hooks
  (emacs-lisp-mode-hook . (enable-paredit-mode
                           aggressive-indent-mode))
  (ielm-mode-hook . (enable-paredit-mode)))

(provide 'module-elisp)
