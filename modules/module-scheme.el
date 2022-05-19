(use-package module-paredit)

(use-package geiser
  ;; :defer t
  :ensure t
  :config
  (setq geiser-active-implementations '(guile)))

(use-package lispy
  :commands (lispy-mode)
  :ensure t)

(use-package scheme-mode
  :defer t
  :hooks ((scheme-mode-hook . (enable-paredit-mode
                               geiser-mode
                               lispy-mode))))
(provide 'module-scheme)
