(require 'module-paredit)
(require 'module-aggressive-indent)

(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)

(use-package emacs-lisp-mode
  :hooks
  (emacs-lisp-mode-hook . (enable-paredit-mode
                           aggressive-indent-mode))
  (ielm-mode-hook . (enable-paredit-mode)))

(provide 'module-elisp)
