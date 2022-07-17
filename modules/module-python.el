(require 'module-lsp)
(require 'module-sp)

(use-package lsp-python-ms :ensure t)

(use-package python-mode
  :hooks
  (python-mode-hook . (lsp-mode
                       smartparens-mode)))

(provide 'module-python)
