(require 'module-paredit)
(require 'module-aggressive-indent)

(use-package geiser
  :ensure t)

(use-package geiser-guile
  :ensure t
  :commands (geiser-guile))

(use-package lispy
  :ensure t)

(use-package scheme-mode
  :hooks
  (scheme-mode-hook . (enable-paredit-mode
                       geiser-mode
                       lispy-mode
                       aggressive-indent-mode)))

(provide 'module-scheme)
