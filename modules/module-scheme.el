(use-package module-paredit)

(use-package geiser
  ;; :defer t
  :ensure t
  :custom
  (geiser-active-implementations '(guile)))

(use-package lispy
  :commands (lispy-mode)
  :ensure t)

(use-package aggressive-indent
  :ensure t)

(use-package ws-butler
  :ensure t)

(use-package scheme-mode
  :defer t
  :hooks ((scheme-mode-hook . (enable-paredit-mode
                               geiser-mode
                               lispy-mode
                               aggressive-indent-mode
                               ws-butler-mode))))
(provide 'module-scheme)
