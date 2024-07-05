(require 'module-lsp)
(require 'module-sp)

(use-package lsp-python-ms :ensure t)

(use-package python-mode
  :extra-modes
  (python-mode . (lsp-mode
                  smartparens-mode)))

(provide 'module-python)
